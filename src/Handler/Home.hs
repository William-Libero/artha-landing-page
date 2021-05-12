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
            
            <div .container>
                <h1>
                    Home

                <button onclick="teste();">
                    OK
        |]

getSobreR :: Handler Html
getSobreR = do
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
            
            <div .container>
                <h1>
                    Sobre

                <button onclick="teste();">
                    OK
        |]

getServicosR :: Handler Html
getServicosR = do
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
            
            <div .container>
                <h1>
                    Servicos

                <button onclick="teste();">
                    OK
        |]