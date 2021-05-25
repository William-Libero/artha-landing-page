{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Cliente where

import Import

formCliente :: Form Cliente
formCliente = renderDivs $ Cliente
    <$> areq textField "Nome: " Nothing
    <*> areq textField "Cpf: " Nothing
    <*> areq intField  "Idade: " Nothing

getClienteR :: Handler Html
getClienteR = do
    (widget,_) <- generateFormPost formCliente
    msg <- getMessage
    defaultLayout $ 
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

                $maybe mensa <- msg 
                    <div>
                        ^{mensa}
                
                <h1>
                    CADASTRO DE CLIENTE
                
                <form method=post action=@{ClienteR}>
                    ^{widget}
                    <input type="submit" value="Cadastrar">
        |]

postClienteR :: Handler Html
postClienteR = do
    ((result,_),_) <- runFormPost formCliente
    case result of 
        FormSuccess cliente -> do 
            runDB $ insert cliente 
            setMessage [shamlet|
                <div>
                    CLIENTE INCLUIDO COM SUCESSO!
            |]
            redirect ClienteR
        _ -> redirect HomeR

-- Select * from cliente where id = cid
-- http://localhost:8080/cliente/perfil/1
getPerfilR :: ClienteId -> Handler Html
getPerfilR cid = do
     cliente <- runDB $ get404 cid
     defaultLayout [whamlet|
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
                                
           <h1>
                PAGINA DE #{clienteNome cliente}
                
           <h2>
                CPF: #{clienteCpf cliente}
                
           <h2>
                Idade: #{clienteIdade cliente}
     |]

-- select * from cliente order by nome;
getListaCliR :: Handler Html
getListaCliR = do
    clientes <- runDB $ selectList [] [Asc ClienteNome]
    defaultLayout $ do 
        $(whamletFile "templates/clientes.hamlet")

postApagarCliR :: ClienteId -> Handler Html
postApagarCliR cid = do
    runDB $ delete cid 
    redirect ListaCliR