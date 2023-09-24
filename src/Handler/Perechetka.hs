{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Perechetka where

import Import

-- Определяем наши данные, которые будут использоваться для создания формы.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

-- Это функция-обработчик для метода запроса GET на HomeR.
-- шаблон ресурса. Все ваши шаблоны ресурсов определены в
-- config/routes.yesodroutes
--
-- Большая часть кода, который вы будете писать в Yesod, находится в этих обработчиках.
-- функции. Вы можете распределить их по нескольким файлам, если хотите, или создать один монолитный файл.
getPerechetkaR :: Handler Html
getPerechetkaR = defaultLayout $ do
        setTitle "Дендрологи" --надпись вкладки браузера
        $(widgetFile "perechetka") -- все файлы Шекспира с этим именем 


