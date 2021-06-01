{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
--import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
--import Text.Julius (RawJS (..))

-- Monad handler => Back-end
-- Monad widget => Front-end
getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        toWidgetHead [julius|
            function teste(){
                alert("oi");
            }
        |]
        toWidgetHead [lucius|
            h1{
                color: red;
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
                            <form method=post action=@{SairR}>
                                <input type="submit" value="Sair">

                <div .container>
                    <h1>
                        Landing-page Artha

                    <button onclick="teste();">
                        OK
        |]