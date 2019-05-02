{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8
import Text.HTML.Scalpel.Core
import Data.Text.Lazy.Encoding (decodeUtf8,encodeUtf8)
import Data.Text.Lazy (strip,toLower,filter,toStrict,fromStrict,Text)
import Data.Text.ICU.Char
import Data.Text.ICU.Normalize
import Database.SQLite.Simple
import Data.Foldable (forM_)
canonicalForm s = noAccents
  where 
     noAccents = Data.Text.Lazy.filter (not . property Diacritic) (fromStrict normalizedText)
     normalizedText = normalize NFD (toStrict s )

removeAccent_tolower = Data.Text.Lazy.toLower . canonicalForm
books_to_download = [("matthew",28,"ΚΑΤΑ ΜΑΤΘΑΙΟΝ"), ("mark",16,"ΚΑΤΑ ΜΑΡΚΟΝ"), ("luke",24,"ΚΑΤΑ ΛΟΥΚΑΝ"), ("john",21,"ΚΑΤΑ ΙΩΑΝΝΗΝ"), ("acts",28,"ΠΡΑΞΕΙΣ ΤΩΝ ΑΠΟΣΤΟΛΩΝ"), ("james",5,"ΕΠΙΣΤΟΛΗ ΙΑΚΩΒΟΥ"), ("peter_1",5,"ΕΠΙΣΤΟΛΗ ΠΕΤΡΟΥ Α'"), ("peter_2",3,"ΕΠΙΣΤΟΛΗ ΠΕΤΡΟΥ Β'"), ("john_1",5,"ΕΠΙΣΤΟΛΗ ΙΩΑΝΝΟΥ Α'"), ("john_2",1,"ΕΠΙΣΤΟΛΗ ΙΩΑΝΝΟΥ Β'"), ("john_3",1,"ΕΠΙΣΤΟΛΗ ΙΩΑΝΝΟΥ Γ'"), ("jude",1,"ΕΠΙΣΤΟΛΗ ΙΟΥΔΑ"), ("romans",16,"ΠΡΟΣ ΡΩΜΑΙΟΥΣ"), ("corinthians_1",16,"Α' ΠΡΟΣ ΚΟΡΙΝΘΙΟΥΣ"), ("corinthians_2",13,"Β' ΠΡΟΣ ΚΟΡΙΝΘΙΟΥΣ"), ("galatians",6,"ΠΡΟΣ ΓΑΛΑΤΑΣ"), ("ephesians",6,"ΠΡΟΣ ΕΦΕΣΙΟΥΣ"), ("philippians",4,"ΠΡΟΣ ΦΙΛΙΠΠΗΣΙΟΥΣ"), ("colossians",4,"ΠΡΟΣ ΚΟΛΟΣΣΑΕΙΣ"), ("thessalonians_1",5,"Α' ΠΡΟΣ ΘΕΣΣΑΛΟΝΙΚΕΙΣ"), ("thessalonians_2",3,"Β' ΠΡΟΣ ΘΕΣΣΑΛΟΝΙΚΕΙΣ"), ("timothy_1",6,"Α' ΠΡΟΣ ΤΙΜΟΘΕΟΝ"), ("timothy_2",4,"Β' ΠΡΟΣ ΤΙΜΟΘΕΟΝ"), ("titus",3,"ΠΡΟΣ ΤΙΤΟΝ"), ("philemon",1,"ΠΡΟΣ ΦΙΛΗΜΟΝΑ"), ("hebrews",13,"ΠΡΟΣ ΕΒΡΑΙΟΥΣ"), ("revelation",22,"ΑΠΟΚΑΛΥΨΙΣ ΙΩΑΝΝΟΥ")]
book (a,_,_) = a
chapter (_,b,_) = [1..b]
title (_,_,c) = c

itemToDownload = concat $ map create_link [(book x,chapter x,title x)|x<-books_to_download]
  where base_address = "http://www.myriobiblos.gr/bible/nt2/"
        create_link (x,y,z) = map (\k->base_address++x++"/"++show k++".asp") y

downloadfile address = do
        url <-address
        response <- httpLBS url 
        putStrLn $ "The status code was: " ++
                    show (getResponseStatusCode response)
        print $ getResponseHeader "Content-Type" response
        let responseBody = decodeUtf8 $ getResponseBody response
        let innerText = scrapeStringLike responseBody $ do 
                                                        chroot "tr"  $ do 
                                                                   text "tr"
                                                                  
        case innerText of
         --(Just content) -> L8.writeFile "ch1.txt"  (encodeUtf8 . strip $ content)
         --(Just content) -> return $ encodeUtf8 . strip $ content
         (Just content) -> return $ strip $ content
         Nothing -> return "" 
analyzeItems = concat $ map (\(lst,s)-> [(i,s)|i<-lst])[(chapter x,title x)|x<-books_to_download]
combineaddress = zipWith (\s (i,s2)-> (s2,i,s)) itemToDownload analyzeItems
actualDownload  = map (\(bookname,chapternumber,address)->(bookname,chapternumber,downloadfile $ parseRequest address))
str_query = "INSERT INTO search_index(bookname,chapternumber,content,accentfree) VALUES(?,?,?,?)"
savecontent = do
              conn<-open "btext.db"
              forM_ (actualDownload combineaddress) $ \(bookname,chapternumber,iocontent)-> do
                                                                           content<-iocontent
                                                                           let accentfree = removeAccent_tolower content
                                                                           execute conn str_query (bookname::Text,chapternumber::Int,content,accentfree)
              close conn
                                       
main :: IO ()
main = --print "hello world" 
       savecontent
