"Because multiple Predictors may watch the same column, each Predictor also has to have a unique random ID. To generate a unique TimeId for a predicted event, we hash together the PredictorId, the RowId of the field it was called for, and the TimeIds of all the most recent changes to the fields the Predictor accessed."

In the hash, why did we include the TimeIds of all the most recent changes to the fields the Predictor accessed? I believe this was a way to maintain uniqueness while avoiding having to hash in **the BaseTime the prediction was at**. This seem desirable because we weren't sure we needed to demand that base times be hashable. However, we now have much stricter requirements of Eq and serializability on user data types.

