{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Sobre where

import Import

getSobreR :: Handler Html
getSobreR = do
    defaultLayout $ do
        toWidgetHead [julius|
        |]
        toWidgetHead [lucius|
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
            
            <div .container>
                    <div .row .justify-content-center .text-center>
                        <h1>
                            Sobre
                    <div .row .justify-content-center .text-center>
                        <h3>
                            Traz segurança para o usuário e familiares
                        <h3>
                            Auxilia profissionais da saúde no momento de atendimento
                        <h3>
                            Facilita reintregação a sociedade, o que influencia na melhora clínica do paciente
                        <h3>
                            Atende aos principios da reforma psiquiatrica
        |]