{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Venda where

import Import
import Database.Persist.Postgresql
import Handler.Auxiliar

formVenda :: ClienteId -> Form Venda
formVenda cid = renderDivs $ Venda
    <$> pure cid
    <*> areq (selectField prodCB) "Produto: " Nothing 
    <*> lift (liftIO (map utctDay getCurrentTime))
    <*> areq intField  "Quantidade: " Nothing

prodCB :: Handler (OptionList (Key Produto))
prodCB = do
    produtos <- runDB $ selectList [] [Asc ProdutoNome]
    optionsPairs $
        map (\r -> (produtoNome $ entityVal r, entityKey r)) produtos

getCompraR :: ClienteId -> Handler Html
getCompraR cid = do
    (widget,_) <- generateFormPost (formVenda cid)
    msg <- getMessage
    defaultLayout $ do
        toWidgetHead [lucius|
            #hident2{
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
                            Cadastro de compra
                    <div .row .justify-content-center .text-center>
                        <form method=post action=@{CompraR cid}>
                            ^{widget}
                            <input type="submit" value="Comprar">
        |]

postCompraR :: ClienteId -> Handler Html
postCompraR cid = do
    ((result,_),_) <- runFormPost (formVenda cid)
    case result of 
        FormSuccess venda -> do 
            runDB $ insert venda 
            setMessage [shamlet|
                <div>
                    COMPRA INCLUIDO COM SUCESSO!
            |]
            redirect (CarrinhoR cid)
        _ -> redirect HomeR

mult :: Double -> Double -> Double
mult = (*)

getCarrinhoR :: ClienteId -> Handler Html
getCarrinhoR cid = do 
    let sql = "SELECT ??,??,?? FROM produto \
          \ INNER JOIN venda ON venda.prodid = produto.id \
          \ INNER JOIN cliente ON venda.cliid = cliente.id \
          \ WHERE cliente.id = ?"
    cliente <- runDB $ get404 cid
    tudo <- runDB $ rawSql sql [toPersistValue cid] :: Handler [(Entity Produto,Entity Venda,Entity Cliente)]
    defaultLayout $ do 
        toWidgetHead [lucius|
            ul{
                list-style: none;
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
            <div .container>
                <div .row .justify-content-center .text-center> 
                    <h1>
                        CARRINHO DE #{clienteNome cliente}
                <div .row .justify-content-center .text-center> 
                    <ul>
                        $forall (Entity _ produto, Entity _ venda, Entity _ _) <- tudo
                            <li>
                                #{produtoNome produto}, #{mult (produtoPreco produto) (fromIntegral (vendaQt venda))} no dia #{show $ vendaDia venda}
        |]