module Handler.Comment where

import Import

postCommentR :: Handler Value
postCommentR = do
    -- requireCheckJsonBody проанализирует тело запроса, приведя его к соответствующему типу, или вернет код состояния 400, если JSON запроса недействителен.
    -- (Экземпляры ToJSON и FromJSON находятся в файле config/models).
    comment <- (requireCheckJsonBody :: Handler Comment)
    --Экземпляр YesodAuth в Foundation.hs определяет UserId как тип, используемый для аутентификации.
    maybeCurrentUserId <- maybeAuthId
    let comment' = comment { commentUserId = maybeCurrentUserId }

    insertedComment <- runDB $ insertEntity comment'
    returnJson insertedComment
