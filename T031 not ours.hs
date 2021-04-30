import DataFile


occursIn x [] = False 
occursIn a (x:xs) = if a==x then True 
                    else occursIn a xs

--- 1) wordToken
fixedString [] = []
fixedString (x:xs) = if occursIn x punct then ' ':x:fixedString xs
                     else x:fixedString xs

wordToken (x:xs) = words(fixedString (x:xs))

--- 2) wordTokenList

wordTokenList [] = []
wordTokenList (x:xs) = wordToken x ++wordTokenList xs


--- unique helper
removeRepeated [] = []
removeRepeated (x:xs) = if occursIn x xs then removeRepeated xs
                        else x:removeRepeated xs
		
--- 3) uniqueBigrams		
uniqueBigrams (x:xs) = removeRepeated(unique(x:xs))

unique [] = []
unique (x:[]) = []
unique (x:y:xs) = (x,y):unique (y:xs)

--- 4) uniqueTrigrams
uniqueTrigrams (x:xs) = removeRepeated(trigram(x:xs))

trigram [] = []
trigram (x:y:[])= []
trigram (x:y:z:xs) = (x,y,z):trigram (y:z:xs)

--- 5) bigramsFreq
bigramsFreq (x:xs) = helperBigramFreq (uniqueBigrams (x:xs)) (x:xs)

helperBigramFreq [] l = []
helperBigramFreq (x:xs) l =  (x,countOcc x (unique l)):helperBigramFreq xs l


countOcc y [] = 0 
countOcc y (x:xs) = if y==x then 1+countOcc y xs
                    else countOcc y xs 

--- 6) trigramsFreq
trigramsFreq (x:xs) = helperTrigramFreq (uniqueTrigrams (x:xs)) (x:xs)

helperTrigramFreq [] (y:ys) = []
helperTrigramFreq (x:xs) (y:ys) =  (x,countOcc x (trigram(y:ys))):helperTrigramFreq xs (y:ys)

---- 7) getFreq

getFreq y [] = 0
getFreq y ((a,b):xs) = if(y==a) then b 
                    else getFreq y xs 
--- 8) generateOneProb				   
getFreq2 y [] = 0
getFreq2 (x,y) (((a,b),i):xs) = if (x,y)==(a,b) then i
                   else getFreq2 (x,y) xs 	
				   
generateOneProb ((a,b,c),i) (x:xs) = i /(getFreq2 (a,b) (x:xs))

--my trial for this 

gp _ [] = 0 
gp ((a,b,c),x) (((a2,b2),y):t) = if a == a2 && b == b2 then x/y
                                 else gp ((a,b,c),x) t

--- 9) genProbPairs
genProbPairs [] (x:xs) = []
genProbPairs (((a,b,c),i):ys) (x:xs) = ((a,b,c),(generateOneProb ((a,b,c),i) (x:xs) ) ):genProbPairs ys (x:xs)

--- 10) generateNextWord
generateNextWord (x:y:[]) (((a,b,c),i):ys) = if (length list)>0 then list!!(randomZeroToX ((length list)-1))
                                             else error "Sorry, it is not possible to infer from current database"
											 where list = (nextWordHelper (x:y:[]) (((a,b,c),i):ys))
nextWordHelper (x:y:[]) [] = []
nextWordHelper (x:y:[]) (((a,b,c),i):ys) = if (x==a && y==b && i>0.03) then c:nextWordHelper (x:y:[]) ys
                                           else nextWordHelper (x:y:[]) ys


-- my take on it 


gnw _ [] = error "Sorry, maaaaaaaate not possible mate "
gnw (a:b:[]) (((a2,b2,r),p):t) = if a == a2 && b == b2 && p>0.03 then r 
                              else gnw (a:b:[]) t   

--- 11) generateText
generateText l i =  l++" "++(textHelper(wordToken l) i)
 
textHelper _ 0 = ""
textHelper ([x,y]) i =   f++" "++(textHelper ([y,f]) (i-1)) where f = helper ([x,y])


helper a = generateNextWord a (fixDocs docs)

fixDocs (x:xs) = genProbPairs ( trigramsFreq(wordTokenList (x:xs)) ) (bigramsFreq(wordTokenList (x:xs)))  

-- my take on this 

gentxt s n = s ++ " "++ (listmaker (wordToken s) n )


listmaker [x,y] n = if n > 0 then  w ++" "++ (listmaker [y,w] (n-1))
                        else "" 
                   where w = generateNextWord [x,y]  (genProbPairs (trigramsFreq(wordTokenList docs)) (bigramsFreq(wordTokenList docs)) )     





-- evaluation part 


mypunct =  ['.','!','?']

sentToken:: String -> [String]
sentToken mylist  =  tokenHelper (wordToken (mylist))

tokenHelper [] = []
tokenHelper (x:xs) = if occursIn x mypunct then  (mystring ++ " " ++ [x] ) : [tokenHelper xs ] 
                    else  mystring ++ x ++" "++ (tokenHelper xs)
               where mystring = " "     
