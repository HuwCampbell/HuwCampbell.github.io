--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Semigroup
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match ("images/*" .||. "js/*") $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.markdown", "resume.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= saveSnapshot "posts"
            >>= loadAndApplyTemplate "templates/default.html" (mathCtx <> postCtx)
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    -- http://jaspervdj.be/hakyll/tutorials/05-snapshots-feeds.html
    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "posts"
            renderAtom feedConfiguration feedCtx posts

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

mathCtx :: Context a
mathCtx = field "katex" $ \item -> do
    katex <- getMetadataField (itemIdentifier item) "katex"
    return $ case katex of
                Just "false" -> ""
                Just "off" -> ""
                _ -> "<link rel=\"stylesheet\" href=\"/css/katex.min.css\">\n\
                     \<script type=\"text/javascript\" src=\"/js/katex.min.js\"></script>\n\
                     \<script src=\"/js/auto-render.min.js\"></script>"

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
   { feedTitle       = "Huw Campbell"
   , feedDescription = "Huw Campbell"
   , feedAuthorName  = "Huw Campbell"
   , feedAuthorEmail = "huw@huwcampbell.com"
   , feedRoot        = "https://huwcampbell.com"
   }
