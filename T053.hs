import DataFile

occursIn x [] = False
occursIn a (x:xs) = if a == x then True
                              else occursIn a xs

removeD[]=[]
removeD(x:xs)=if ( occursIn x  xs)then removeD xs
else x:removeD xs



uniqueBigrams []=[] 

uniqueBigrams [x]=[]
uniqueBigrams(x1:x2:xs)=removeD(((splitAt (length x1) (unwords [x1,x2]))) :uniqueBigrams(x2:xs))




makelist ( (x:xs),(y:ys) )=[(x:xs),(y:ys)]



wordToken []=[]
wordToken (x:xs)= wordTokenh(words (x:xs))


wordTokenh[]=[] 
wordTokenh (x:xs)=if( occursIn (last x) punct )then (makelist(break (==last x) x))++ wordTokenh xs else x:wordTokenh xs



wordTokenList :: [String] -> [String]

wordTokenList[]=[]
wordTokenList(x:xs)= (wordToken x)++(wordTokenList xs)



uniqueTrigrams[]=[]
uniqueTrigrams[x]=[]
uniqueTrigrams[x,y]=[]
uniqueTrigrams(x1:x2:x3:xs)= removeD( [(x1,x2,x3)]++uniqueTrigrams(x2:x3:xs))



uniqueBigrams2 [x]=[]
uniqueBigrams2(x1:x2:xs)=((splitAt (length x1) (unwords [x1,x2]))) :uniqueBigrams(x2:xs)



bigramsFreq[]=[]
bigramsFreq(x:xs)= bigramsFreqh(uniqueBigrams2(x:xs))




bigramsFreqh[]=[]
bigramsFreqh (x:xs)=[(x,occursIn2 x (x:xs))]++bigramsFreqh (filter (/=x) (x:xs))




occursIn2 a [] = 0
occursIn2 a (x:xs) = if a == x then 1+(occursIn2 a (xs))
        else occursIn2 a xs

      

uniqueTrigrams2[]=[]
uniqueTrigrams2[x]=[]
uniqueTrigrams2[x,y]=[]
uniqueTrigrams2(x1:x2:x3:xs)= [(x1,x2,x3)]++uniqueTrigrams(x2:x3:xs)

      

trigramsFreq[]=[]
trigramsFreq(x:xs)= trigramsFreqh(uniqueTrigrams2(x:xs))




trigramsFreqh[]=[]
trigramsFreqh (x:xs)=[(x,occursIn2 x (x:xs))]++trigramsFreqh (filter (/=x) (x:xs))



getFreq y [] = 0 
getFreq y	((x,n):xs) = if y == x then n
                      else getFreq y xs
        
        

-- generateOneProb _ [] = 0	
--   generateOneProb ((a,b,m),count) (((x,y),count2):xs) = if a == x && b == y then count / count2
--	  	else generateOneProb ((a,b,m),count) xs


areEqual ((a,b,m),count) ((x,y),count2)=  if a == x && b == y then True else False


generateOneProb _ [] = 0  
generateOneProb y (x:xs) = if areEqual y x then (getCount1 y) / (getCount x)
  else generateOneProb y xs


getCount1 ((a,b,m),count) = count
getCount ((a,b),count) = count

genProbPairs :: Fractional a => [((String,String,String),a)] ->[((String,String),a)] -> [((String,String,String),a)]

genProbPairs [] (y:ys) = []
genProbPairs (x:xs) (y:ys) = (yarab  x (y:ys)):genProbPairs xs (y:ys)

getTriGrams ((a,b,c),count) = (a,b,c)

yarab  x (y:ys)= (getTriGrams x ,generateOneProb x (y:ys))



check [x1,x2] [] = []
check [x1,x2] (((a,b,c),count):ys) = if a==x1 && b==x2 && count> 0.03 then (c: check ([x1,x2]) ys)                                            
                                                                      else check ([x1,x2]) ys 

generateNextWord x y = if check x y ==[] then error "Sorry, it is not possible to infer from current database" 
                                      else (check x y)!!(randomZeroToX (length((check x y))-1))
                  




gentxt s n = s ++ " "++ (listmaker (wordToken s) n )

listmaker [x,y] n = if n > 0 then  w ++" "++ (listmaker [y,w] (n-1))
                  else "" 
            where w = generateNextWord [x,y]  (genProbPairs (trigramsFreq(wordTokenList docs)) (bigramsFreq(wordTokenList docs)) )     