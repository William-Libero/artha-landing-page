{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Medico where

import Import

formMedico :: Form Medico
formMedico = renderDivs $ Medico
    <$> areq textField "Nome: " Nothing
    <*> areq textField "Cpf: " Nothing
    <*> areq intField  "Idade: " Nothing

getMedicoR :: Handler Html
getMedicoR = do
    (widget,_) <- generateFormPost formMedico
    msg <- getMessage
    defaultLayout $ do
        toWidgetHead [lucius|
            #hident2{
                margin-left: 0.5%;
            }

            #hident3{
                margin-left: 2%;
            }

            #hident4{
                margin-left: 0.8%;
            }
        |]
        [whamlet|
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

                $maybe mensa <- msg 
                    <div>
                        ^{mensa}
            <div .container>
                <div .row .justify-content-center .text-center>        
                    <h1>
                        Cadastro de médico
                <div .row .justify-content-center .text-center>
                    <form method=post action=@{MedicoR}>
                        ^{widget}
                        <input type="submit" value="Cadastrar">
        |]

postMedicoR :: Handler Html
postMedicoR = do
    ((result,_),_) <- runFormPost formMedico
    case result of 
        FormSuccess medico -> do 
            runDB $ insert medico 
            setMessage [shamlet|
                <div>
                    medico INCLUIDO COM SUCESSO!
            |]
            redirect MedicoR
        _ -> redirect HomeR

-- Select * from medico where id = cid
-- http://localhost:8080/medico/perfil/1
getPerfilMedR :: MedicoId -> Handler Html
getPerfilMedR cid = do
     medico <- runDB $ get404 cid
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
                Página do médico: #{medicoNome medico}
                
           <h2>
                CPF: #{medicoCpf medico}
                
           <h2>
                Idade: #{medicoIdade medico}
     |]

-- select * from medico order by nome;
getListaMedR :: Handler Html
getListaMedR = do
    medicos <- runDB $ selectList [] [Asc MedicoNome]
    defaultLayout $ do 
        $(whamletFile "templates/medicos.hamlet")

postApagarMedR :: MedicoId -> Handler Html
postApagarMedR mid = do
    runDB $ delete mid 
    redirect ListaMedR