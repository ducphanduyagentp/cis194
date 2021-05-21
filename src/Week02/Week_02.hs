{-# OPTIONS_GHC -Wall #-}

module Week02.Week_02 
    ( week02Test
    ) where 

import Week02.LogAnalysis
import Week02.Log

week02Test :: IO()
week02Test = do
    print ("==== BEGIN WEEK 02 ====")
    print (parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help")
    print (parseMessage "I 29 la la la" == LogMessage Info 29 "la la la")
    print (parseMessage "This is not in the right format" == Unknown "This is not in the right format")
    print ("==== ====")
    list <- (testParse parse 11 "sample.log")
    print (list)
    print ("==== ====")
    print (build list)
    print ("==== ====")
    print (inOrder (build list))
    print ("==== ====")
    print (whatWentWrong list)