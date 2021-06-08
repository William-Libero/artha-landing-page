{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Admin where

import Import

formAdmin :: Form Admin
formAdmin = renderDivs $ Admin
    <$> areq textField "E-Mail: " Nothing
    <*> areq textField "Senha: " Nothing

getAdminR :: Handler Html
getAdminR = do
    (widget,_) <- generateFormPost formAdmin
    msg <- getMessage
    defaultLayout $ do
        toWidgetHead [lucius|
            #hident2{
                margin-left: 0.5%;
            }

            #hident3{
                margin-left: 0.7%;
            }
        |]
        [whamlet|
            <div .container>
                <div .navbar .navbar-expand-lg .navbar-light .bg-light>
                    <a .navbar-brand href="/">
                        <img src=@{StaticR img_logoartha_ico} width="30" height="30">
                    <ul .navbar-nav .mr-auto>
                        <li .nav-item .active>
                            <a .nav-link href="/">
                                Home
                        <li .nav-item .active>
                            <a .nav-link href="/sobre">
                                Sobre
                        <li .nav-item .active>
                            <a .nav-link href="/servicos">
                                Serviços
                        <li .nav-item .active>
                            <a .nav-link href="/cliente">
                                Incluir Cliente
                        <li .nav-item .active>
                            <a .nav-link href="/clientes">
                                Clientes
                        <li .nav-item .active>
                            <a .nav-link href="/medico">
                                Incluir medico
                        <li .nav-item .active>
                            <a .nav-link href="/medicos">
                                Medicos
                        <li .nav-item .active>
                            <a .nav-link href="/admin">
                                Incluir admin
                        <li .nav-item .active>
                            <a .nav-link href="/admins">
                                Admins
                        <li .nav-item .active>
                            <a .nav-link href="/produtos">
                                Produtos
                        <li .nav-item .active>
                            <form method=post action=@{SairR}>
                                <input type="submit" value="Sair">

                $maybe mensa <- msg 
                    <div>
                        ^{mensa}

            <div .container>
                <div .row .justify-content-center .text-center>
                    <h1>
                        CADASTRO DE ADMIN
                <div .row .justify-content-center .text-center>
                    <form method=post action=@{AdminR}>
                        ^{widget}
                        <input type="submit" value="Cadastrar">
        |]

postAdminR :: Handler Html
postAdminR = do
    ((result,_),_) <- runFormPost formAdmin
    case result of 
        FormSuccess admin -> do 
            runDB $ insert admin 
            setMessage [shamlet|
                <div>
                    ADMIN INCLUIDO COM SUCESSO!
            |]
            redirect AdminR
        _ -> redirect HomeR

-- Select * from admin where id = cid
-- http://localhost:8080/admin/perfil/1
getPerfilAdminR :: AdminId -> Handler Html
getPerfilAdminR aid = do
     admin <- runDB $ get404 aid
     defaultLayout [whamlet|
        <div .container>
            <div .navbar .navbar-expand-lg .navbar-light .bg-light>
                <a .navbar-brand href="/home">
                    <img src=@{StaticR img_logoartha_ico} width="30" height="30">
                <ul .navbar-nav .mr-auto>
                    <li .nav-item .active>
                        <a .nav-link href="/home">
                            Home
                    <li .nav-item .active>
                        <a .nav-link href="/sobre">
                            Sobre
                    <li .nav-item .active>
                        <a .nav-link href="/servicos">
                            Serviços
                    <li .nav-item .active>
                        <a .nav-link href="/cliente">
                            Incluir Cliente
                    <li .nav-item .active>
                        <a .nav-link href="/clientes">
                            Clientes
                    <li .nav-item .active>
                        <a .nav-link href="/medico">
                            Incluir medico
                    <li .nav-item .active>
                        <a .nav-link href="/medicos">
                            Medicos
                    <li .nav-item .active>
                        <a .nav-link href="/admin">
                            Incluir admin
                    <li .nav-item .active>
                        <a .nav-link href="/admins">
                            Admins
                    <li .nav-item .active>
                        <a .nav-link href="/produtos">
                            Produtos
                    <li .nav-item .active>
                        <form method=post action=@{SairR}>
                            <input type="submit" value="Sair">
           <h1>
                PAGINA DE #{adminEmail admin}
     |]

-- select * from admin order by nome;
getListaAdminR :: Handler Html
getListaAdminR = do
    admins <- runDB $ selectList [] [Asc AdminId]
    defaultLayout $ do 
        $(whamletFile "templates/admin.hamlet")

postApagarAdminR :: AdminId -> Handler Html
postApagarAdminR cid = do
    runDB $ delete cid 
    redirect ListaAdminR