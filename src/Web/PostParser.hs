{-# LANGUAGE OverloadedStrings #-}

module Web.PostParser where

import Prelude hiding (div, id)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Char (isLetter, isNumber)
import Data.Time

import Database.Persist.Sql (fromSqlKey)

import Text.Blaze.Html (toHtml, toValue, textValue, preEscapedToHtml)
import Text.Blaze.Html5 (Html, (!), div, img, ul, ol, li, a, i ,b, link,
                         br, h1)
import Text.Blaze.Html5.Attributes (href, rel, src, type_, class_, id, target)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Model.Types
import Web.BetterMdParser

renderPost :: Post -> Int -> T.Text
renderPost post renderType =
  toStrict $ renderHtml $ parsePost post renderType

renderDrivelPost :: (PostId, Post) -> UTCTime -> T.Text
renderDrivelPost (pid, post) now = toStrict $ renderHtml $ do
  putHtml (drivelTitleLine (postTitle post) (postCategories post)
           (postCrtDate post) now tpid)
  case postPtype post of
    1 -> do putHtml $ parseFirstPar $ postContent post
            div ! class_ "readmorecont" $ a ! class_ "readmorelink"
              ! href (textValue $ T.concat ["/drivel/post/", tpid]) $
              "Read the rest of this post »"
    _ -> putHtml $ parseContent 2 $ postContent post
  where tpid = T.pack $ show $ fromSqlKey pid

parsePost :: Post -> Int -> Html
parsePost post 0 = putHtml $ parseContent 0 (postContent post) -- stc site
parsePost post 1 = do
  putHtml (postTitleLine (postTitle post) (postCategories post)
           (postCrtDate post))
  putHtml $ parseContent 1 (postContent post)
parsePost post 2 = do
  putHtml $ postTitleLine Nothing Nothing (postCrtDate post)
  putHtml $ parseContent 2 (postContent post)
parsePost post _ = do
  putHtml $ parseContent 3 (postContent post)

putHtml (Just h) = h
putHtml Nothing = return ()

drivelTitleLine :: Maybe T.Text -> Maybe [T.Text] -> UTCTime -> UTCTime
                -> T.Text -> Maybe Html
drivelTitleLine (Just title) (Just cats) crt _ postId = Just $ do
  div ! class_ "driveltitlebar" $ do
    div ! class_ "driveltitlecont" $ div ! class_ "driveltitle" $
      (a ! href (textValue $ T.concat ["/drivel/post/", postId])
       ! class_ "plink" $ (toHtml title))
    div ! class_ "drivelcats" $ toHtml (map cToHtml cats)
    div ! class_ "drivelcrtdate" $ (toHtml $ formatTime
                                    defaultTimeLocale "%d. %b. %Y" crt)
drivelTitleLine _ _ crt now _ = Just $ do
  div ! class_ "drivelemptybar" $ (toHtml $ (timediff now crt) ++ " ago")

timediff now crt
  | raw <= 60 = (show $ conv raw) ++ " sec" ++ (plr $ conv raw)
  | raw > 60*60*24*7 = (show $ conv raw) ++ " week" ++ (plr $ conv raw)
  | raw > 60*60*24 = (show $ conv raw) ++ " day" ++ (plr $ conv raw)
  | raw > 60*60 = (show $ conv raw) ++ " hour" ++ (plr $ conv raw)
  | raw > 60 = (show $ conv raw) ++ " min" ++ (plr $ conv raw)
  where raw = diffUTCTime now crt
        conv raw | raw <= 60 = floor raw
                 | raw > 60*60*24*7 = floor (raw / (60*60*24*7))
                 | raw > 60*60*24 = floor (raw / (60*60*24))
                 | raw > 60*60 = floor (raw / (60*60))
                 | raw > 60 = floor (raw / 60)
        plr r | r == 1 = ""
              | otherwise = "s"

postTitleLine :: Maybe T.Text -> Maybe [T.Text] -> UTCTime -> Maybe Html
postTitleLine (Just title) (Just cats) crt = Just $ do
  div ! class_ "posttitlebar" $ do
    div ! class_ "posttitlebarbar" $ do
      div ! class_ "posttitle" $ toHtml title
      div ! class_ "postcats" $ toHtml (map cToHtml cats)
      div ! class_ "postcrtdate" $ (toHtml $ formatTime
                                    defaultTimeLocale "%d. %b. %Y" crt)
postTitleLine _ _ crt = Just $ do
  div ! class_ "posttitlebar" $ do
    div ! class_ "postdriveltitle" $ "Drivel entry from "
    div ! class_ "postdrivelcrtdate" $ (toHtml $ formatTime
                                        defaultTimeLocale "%d. %b. %Y" crt)
    ":"


cToHtml cat = div ! class_ "postcat" $ toHtml (T.concat [cat, " "])

parseContent :: Int -> T.Text -> Maybe Html
parseContent tp cont = fmap toHtml $ (docToHtml tp (parseMd cont))

docToHtml :: Int -> Doc -> Maybe [Html]
docToHtml _ [] = Nothing
docToHtml tp doc = Just $ map (secToHtml tp)
                   (zip doc ((tail doc) ++ [last doc]))

secToHtml :: Int -> (Sec, Sec) -> Html
secToHtml _ ((Par bs), (Hdl (n, _))) =
  div ! class_ (toValue ("par par" ++ show n)) $ toHtml $ map blockToHtml bs
secToHtml _ ((Par bs), _) = div ! class_ "par" $ toHtml $ map blockToHtml bs
secToHtml 0 ((Hdl (n, es)), _) =
  h1
  ! class_ (textValue (T.append "hdls" $ T.pack $ (show n)))
  ! id (textValue $ T.concat $ map eleToPlain es) $
  toHtml $ map eleToHtml es
secToHtml _ ((Hdl (n, es)), _) =
  h1
  ! class_ (textValue (T.append "hdl" $ T.pack $ (show n)))
  ! id (textValue $ T.concat $ map eleToPlain es) $
  toHtml $ map eleToHtml es
secToHtml _ ((Htm t), _) = preEscapedToHtml t

blockToHtml :: Block -> Html
blockToHtml (List els) = ul ! class_ "ul" $ toHtml $ map toListEle els
blockToHtml (Enum (n, els)) = ol ! class_ "ol" $ toHtml $ map toListEle els
blockToHtml (Norm els) = toHtml $ mapElesToHtml els

toListEle :: [Ele] -> Html
toListEle els = li ! class_ "li" $ toHtml $ map eleToHtml els

mapElesToHtml :: [Ele] -> [Html]
mapElesToHtml [] = []
mapElesToHtml ((Plain t) : (Image (d, l)) : es) =
  [eleToHtml (Plain t)
  , br
  , img ! (src $ toValue l) ! class_ "imag"
  ] ++ mapElesToHtml es
mapElesToHtml (e : es) = [eleToHtml e] ++ mapElesToHtml es

eleToHtml :: Ele -> Html
eleToHtml Newline = br
eleToHtml (Plain t) = toHtml t
eleToHtml (Italic t) = i $ toHtml t
eleToHtml (Bold t) = b $ toHtml t
eleToHtml (Latex1 t) = toHtml $ T.concat ["$", t, "$"]
eleToHtml (Latex2 t) = toHtml $ T.concat ["$$", t, "$$"]
eleToHtml (Link (d, l)) = a ! (href $ toValue l) ! class_ "link"
                          ! target "blank" $ toHtml d
eleToHtml (Image (d, l)) = do
  img ! (src $ toValue l) ! class_ "imag_nomargin"

eleToPlain :: Ele -> T.Text
eleToPlain Newline = ""
eleToPlain (Plain t) = T.filter lon t
eleToPlain (Italic t) = T.filter lon t
eleToPlain (Bold t) = T.filter lon t
eleToPlain (Latex1 t) = T.concat ["$", T.filter lon t, "$"]
eleToPlain (Latex2 t) = T.concat ["$$", T.filter lon t, "$$"]
eleToPlain (Link (d, l)) = T.filter lon d
eleToPlain (Image (d, l)) = T.filter lon d

lon c = isLetter c || isNumber c

parseFirstPar :: T.Text -> Maybe Html
parseFirstPar cont = fmap toHtml (docFirstPar $ parseMd cont)

docFirstPar :: Doc -> Maybe Html
docFirstPar [] = Nothing
docFirstPar ((Par bs) : _) = Just (div ! class_ "par" $ (toHtml $ fmap
                                                         blockToHtml bs))
docFirstPar (_ : doc) = docFirstPar doc
