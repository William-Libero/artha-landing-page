{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Login where

import Import
import Handler.Auxiliar

formLoginAdminR :: Form Admin
formLoginAdminR = renderDivs $ Admin
    <$> areq textField "E-Mail: " Nothing
    <*> areq passwordField "Senha: " Nothing

getLoginAdminR :: Handler Html
getLoginAdminR  = do
    (widget,_) <- generateFormPost formLoginAdminR
    msg <- getMessage
    defaultLayout $ 
        [whamlet|
            <div .container>
                <div .navbar .navbar-expand-lg .navbar-light .bg-light>
                    <a .navbar-brand href="/home">
                        <img src=@{StaticR img_logoartha_ico} width="30" height="30">
                    <ul .navbar-nav .mr-auto>
                        <li .nav-item .active>
                            <a .nav-link href="/admin">
                                Incluir admin
                        <li .nav-item .active>
                            <a .nav-link href="/admins">
                                Admins
                        <li .nav-item .active>
                            <form method=post action=@{SairR}>
                                <input type="submit" value="Sair">

                $maybe mensa <- msg 
                    <div>
                        ^{mensa}

                <h1>
                    Login

                <form method=post action=@{LoginAdminR}>
                    ^{widget}
                    <input type="submit" value="Logar">
        |]

postLoginAdminR :: Handler Html
postLoginAdminR = do
    ((result ,_),_) <- runFormPost formLoginAdminR
    case result of
        FormSuccess (Admin email senha) -> do
            adminExiste <- runDB $ getBy (UniqueEmail email)
            case adminExiste of
                Nothing -> do
                    setMessage [shamlet|
                        Usuário não cadastrado!
                    |]
                    redirect LoginAdminR
                Just (Entity _ admin) -> do
                    if senha == adminSenha admin then do
                        setSession "_ID" (adminEmail admin)
                        redirect HomeR
                    else do
                        setMessage [shamlet|
                            Usuário e/ou senha não confere!
                        |]
                        redirect LoginAdminR
        _ -> redirect HomeR

postSairR :: Handler Html
postSairR = do
    deleteSession "_ID"
    redirect LoginAdminR