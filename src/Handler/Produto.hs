{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Produto where

import Import
import Handler.Auxiliar

formProduto :: Maybe Produto -> Form Produto
formProduto ms = renderDivs $ Produto
    <$> areq textField "Nome: " Nothing
    <*> areq doubleField "Preço: "  Nothing

getProdutoR :: Handler Html
getProdutoR = do
    (widget,_) <- generateFormPost (formProduto Nothing)
    msg <- getMessage
    defaultLayout $ do
        toWidgetHead [lucius|
            #hident2{
                margin-left: 0.5%;
            }

            #hident3{
                margin-left: 0.5%;
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
                            Cadastro de produto
                    <div .row .justify-content-center .text-center>
                        <form method=post action=@{ProdutoR}>
                            ^{widget}
                            <input type="submit" value="Cadastrar">
        |]

postProdutoR :: Handler Html
postProdutoR = do
    ((result,_),_) <- runFormPost (formProduto Nothing)
    case result of 
        FormSuccess produto -> do 
            runDB $ insert produto 
            setMessage [shamlet|
                <div>
                    PRODUTO INCLUIDO COM SUCESSO!
            |]
            redirect ProdutoR
        _ -> redirect HomeR