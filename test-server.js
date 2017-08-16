var fs = require('fs');
var http = require('http');
var process = require('process');

var config = {
  staticFiles: {
    path: '.',
    reloadInterval: 1000, // * 60 * 5,
    logReload: false,
    extensionMap: {
      html: 'text/html',
      css: 'text/css',
      js: 'text/javascript',
      glsl: 'text/x-glsl',
    },
  },
  http: {
    port: 8000,
  },
};

var util = {};

util.isObject = function(scrutinent) {
  var type = typeof scrutinent;
  return type === 'function' || (type === 'object' && !!scrutinent);
}

util.extend = function(destination, source) {
  for (var key in source) {
    destination[key] = source[key];
  }
  return destination;
};

util.deepExtend = function(destination, source) {
  for (var key in source) {
    if (util.isObject(destination[key]) && util.isObject(source[key])) {
      destination[key] = util.deepExtend(destination[key], source[key]);
    } else {
      destination[key] = source[key];
    }
  }
  return destination;
};

util.logErrors = function(inner) {
  try {
    return inner.apply(this, Array.prototype.slice.call(arguments, 1));
  } catch(e) {
    console.log(e);
  }
};

var staticFiles = {};

function logNotPlainOrDirectory(path) {
  console.log(
      'Not a plain file or directory "'
      + path
      + '"; treating as absent.');
};

function logNotDirectory(path) {
  console.log(
      'Not a directory "'
      + path
      + '"; leaving in prior state.');
};

function logNoReadAccess(path) {
  console.log(
      'No read access to "'
      + path
      + '"; treating as absent.');
};

function logReadError(path, error) {
  console.log(
      'Error ('
      + error
      + ') while reading "'
      + path
      + '"; leaving in prior state.');
};

function isIgnoredFilename(filename) {
  return /^\./.test(filename);
};

staticFiles.load = function(config, state) {
  var basePath = config.path || '';
  var reloadInterval = config.reloadInterval || 30000;
  var shouldLogReload = config.logReload;

  state = state || {};

  if (!state.resourceData) {
    state.resourceData = {};
  }

  if (!state.extensionMap) {
    state.extensionMap = [];

    for(var extension in config.extensionMap) {
      state.extensionMap.push(new RegExp('\\.' + extension + '$'));
      state.extensionMap.push(config.extensionMap[extension]);
    }
  }

  var log;
  if (shouldLogReload) {
    logReload = console.log;
  } else {
    logReload = function() {};
  }

  logReload('Loading static files from "' + basePath + '".');

  var newResources = {};
  var outstandingItems = 0;

  var maybeDone = function() {
    outstandingItems--;
    if (outstandingItems > 0) return;

    for (var subpath in state.resourceData) {
      if (newResources[subpath] === undefined) {
        console.log('Discarded resource "' + subpath + '".');
        delete state.resourceData[subpath];
      }
    }

    for (var subpath in newResources) {
      state.resourceData[subpath] = newResources[subpath];
    }

    logReload('Done loading static files; will reload again in '
        + reloadInterval + 'ms.');
    setTimeout(staticFiles.load, reloadInterval, config, state);
  }

  var populate = function(fullPath, subpath, timestamp, error, data) {
    if (!error) {
      var contentType = 'text/html';
      for (var i = 0; i < state.extensionMap.length; i += 2) {
        if (state.extensionMap[i].test(subpath)) {
          contentType = state.extensionMap[i+1];
          break;
        }
      }

      newResources[subpath] = {
        timestamp: timestamp,
        data: data,
        contentType: contentType,
      };

      logReload('Loaded resource "' + subpath + '" as MIME type ' + contentType + '.');
    } else {
      newResources[subpath] = state.resourceData[subpath];
      logReadError(fullPath, error);
    }
    maybeDone();
  };

  var loadOne = function(subpath, error) {
    var fullPath = basePath + subpath;

    if (!error) {
      var stat = fs.lstatSync(fullPath);
      if (stat.isFile()) {
        var timestamp = stat.mtime;
        var oldTimestamp =
            state.resourceData[subpath] &&
            state.resourceData[subpath].timestamp;
        if (!oldTimestamp || timestamp.getTime() > oldTimestamp.getTime()) {
          fs.readFile(fullPath,
              populate.bind(this, fullPath, subpath, timestamp));
        } else {
          newResources[subpath] = state.resourceData[subpath];
          maybeDone();
        }
      } else if (stat.isDirectory()) {
        var items = fs.readdirSync(fullPath);
        for (var i in items) {
          var item = items[i];
          if (!isIgnoredFilename(item)) {
            var itemSubpath = subpath + '/' + item;
            var itemFullPath = basePath + itemSubpath;
            outstandingItems++;
            fs.access(itemFullPath, fs.R_OK, loadOne.bind(this, itemSubpath));
          }
        }
        maybeDone();
      } else {
        logNotPlainOrDirectory(fullPath);
        maybeDone();
      }
    } else {
      console.log(error);
      logNoReadAccess(fullPath);
      maybeDone();
    }
  };

  outstandingItems++;
  fs.access(basePath, fs.R_OK, function(error) {
    if (!error) {
      var stat = fs.lstatSync(basePath);
      if (stat.isDirectory()) {
        var items = fs.readdirSync(basePath);
        for (var i in items) {
          var item = items[i];
          if (!isIgnoredFilename(item)) {
            var itemSubpath = '/' + item;
            var itemFullPath = basePath + itemSubpath;
            outstandingItems++;
            fs.access(itemFullPath, fs.R_OK, loadOne.bind(this, itemSubpath));
          }
        }
      } else {
        logNotDirectory(basePath);
      }
    } else {
      prefixesToRemove.push('/');
      logNoReadAccess(basePath);
    }
    maybeDone();
  });
};

var methods = {};

function Server(config) {
  this.config = config;
  this.resources = {};

  this.resources.staticFiles = {};
  staticFiles.load(config.staticFiles, this.resources.staticFiles);

  httpServer = http.createServer();
  for (var event in methods) {
    httpServer.on(event, util.logErrors.bind(
        this, methods[event], this.config.http));
  }
  httpServer.listen(this.config.http.port);

  if (this.config.process && this.config.process.user !== undefined) {
    process.setuid(this.config.process.user);
  }
  console.log('Running as ' + process.getuid());
}

Server.prototype.giveUp = function(response) {
  response.writeHead(500, {
    'Content-Length': 0,
    'Content-Type': 'text/html',
  });
}

Server.prototype.tryResource = function(response, status, resourceName) {
  var resource = this.resources.staticFiles.resourceData[resourceName];

  if (resource) {
    console.log('Found "' + resourceName + '".');
    response.writeHead(status, {
      'Content-Length': resource.data.length,
      'Content-Type': resource.contentType,
    });
    response.write(resource.data);
  } else if (status == 200) {
    console.log('Didn\'t find "' + resourceName + '".');
    this.tryResource(response, 404, '/404');
  } else {
    console.log('Oh noes.');
    this.giveUp(response);
  }
}

methods.request = function(config, request, response) {
  try {
    if (request.method == 'GET') {
      if (!/^\//.test(request.url)) {
        this.tryResource(response, 404, '/404');
      } else if (/\/$/.test(request.url)) {
        this.tryResource(response, 200, request.url + 'index.html');
      } else if (/\/index\.html$/.test(request.url)) {
        response.writeHead(301, {
          'Location': request.url.replace(/\/index\.html$/, '/'),
        });
      } else {
        this.tryResource(response, 200, request.url);
      }
    }
  } finally {
    if (!response.headersSent) {
      this.giveUp(response);
    }
    response.end();
  }
}

methods.listening = function(config) {
  console.log('Listening on port ' + config.port);
}

new Server(config);

