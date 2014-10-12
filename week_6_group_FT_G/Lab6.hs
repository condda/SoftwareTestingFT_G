module Lab6

where
import Data.List
import System.Random
import Week6
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)
import Data.Bits

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]

-- Assignment 1

testExM :: Integer -> Integer -> Integer -> Bool
testExM a b c = (b <= 0) || (c <= 0) || (exM a b c) == (oldExM a b c)


-- Assignment 2

-- using :set +s for timing, I got the following results:

-- oldExM 100 (2^24) 10 took 1.36 secs,       159623272 bytes

-- oldExM 100 ((2^24)-1) 10 took 2.53 secs,   261426480 bytes

-- exM 100 (2^24) 10 took 0.00 secs,          2059024 bytes

-- exM 100 ((2^24)-1) 10 took 0.00 secs,      2605976 bytes




-- oldExM 100 (2^26) 10 took 11.94 secs,      628208792 bytes

-- oldExM 100 ((2^26)-1) 10 took 11.94 secs,  1034963032 bytes

-- exM 100 (2^26) 10 took 0.00 secs,          2059040 bytes

-- exM 100 ((2^26)-1) 10 took 0.01 secs,      2575176 bytes

-- Assignment 3
composites :: [Integer]
composites = composites' primes
  where composites' (p1:p2:ps) = [p1+1..p2-1] ++ composites' (p2:ps)

-- Very simple test using the first 105 composites from http://en.wikipedia.org/wiki/Composite_number
testComposites = take 105 composites == [ 4, 6, 8, 9, 10, 12, 14, 15, 16, 18, 20, 21, 22, 24, 25, 26, 27, 28, 30, 32, 33, 34, 35, 36, 38, 39, 40, 42, 44, 45, 46, 48, 49, 50, 51, 52, 54, 55, 56, 57, 58, 60, 62, 63, 64, 65, 66, 68, 69, 70, 72, 74, 75, 76, 77, 78, 80, 81, 82, 84, 85, 86, 87, 88, 90, 91, 92, 93, 94, 95, 96, 98, 99, 100, 102, 104, 105, 106, 108, 110, 111, 112, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 128, 129, 130, 132, 133, 134, 135, 136, 138, 140 ]

-- Assignment 4

-- As the composites are the numbers that are not prime, we can
-- test Fermat's Primality Check by checking if k is "very" high.

-- Test if "probable prime" can be found in the list of numbers
testP k (n:ns) = do
  f <- primeF k n
  if f
    then return n
    else testP k ns

testF k = testP k composites

-- With testF 1, we got numbers between 4 and 63 after 20 runs.
testCompF _ 0 k nos = testP k nos
testCompF fn times k nos = do
  f <- testP k nos
  g <- testCompF fn (times - 1) k nos
  return $ if f `fn` g then f else g

-- testCompF (<) 100 1 composites    resulted in 4    (0.00 secs, 1577184 bytes)
-- testCompF (>) 100 1 composites    resulted in 63   (0.01 secs, 3150840 bytes)

-- testCompF (<) 100 2 composites    resulted in 4    (0.36 secs, 176821472 bytes)
-- testCompF (>) 100 2 composites    resulted in 1417 (0.25 secs, 144077600 bytes)

-- testCompF (<) 100 3 composites    resulted in 4    (4.20 secs, 962954240 bytes)
-- testCompF (>) 100 3 composites    resulted in 8911 (4.96 secs, 1197929216 bytes)

-- As we can see from the results above, the chance of finding a prime number is higher for a higher k.
-- However, also memory and time increases dramatically. This is logically explainable:
-- the memory used for the function primeF is O(3k), because k is the amount of times
-- primeF is called internally, with 0 just returning True. Also, the more precisely prime numbers
-- can be found, the more calls are made to primeF, because testMinF exits when the first
-- composite is found.



-- Assignment 5 and 6
b2i b = if b then 1 else 0

testAl _ _ [] = return 0
testAl al k (n:nos) = do
  x <- al k n
  y <- testAl al k nos
  return $ y + (b2i $ x && (not $ isPrime n))

testFPNP_probability k n = testAl primeF k (take n carmichael)
testMRPNP_probability k n = testAl primeMR k (take n carmichael)

-- testFPNP_probability k 200 resulted in 199 or 200 for a k between 1 and 3, which means
-- that for almost all cases, primeF thinks that a carmichael number may be a prime, while
-- that is not the case. Even with a k of 100, the result did not go lower than 191.

-- testMRPNP_probability 3 200 resulted in 0, which means that for (almost) all cases,
--   primeMR thinks that a carmichael number is NOT a prime.

-- testMRPNP_probability 1 200, however, resulted in the range between 16 and 25 after a few attempts.


-- Assignment 6
testMRPNP _ [] = return 0

testMRPNP k (n:nos) = do
  x <- primeMR k n
  y <- testMRPNP k nos
  return $ y + (b2i $ x && (not $ isPrime n))

-- testMRPNP 3 (take 100 carmichael) counts the amount of times
-- that a number is prime, and considered a prime by the primeMR function.
-- Even after running this function multiple times, the result was always 0,
-- meaning that the primeMR-function does not recognize the carmichael
-- numbers as possible prime numbers.

-- Assignment 7
mersP p = return $ (2 ^ p) - 1

-- The first thing we tried, is that
-- we created a function for quickly finding large prime numbers, which
-- does not just iterate through the list of prime numbers, but takes
-- the found prime number and puts it back into the function.
mersennePrime1 _ p 0 = return p
mersennePrime1 k p steps = do
  newP <- mersP p
  isP <- primeMR k p
  if isP then
    mersennePrime1 k newP (steps - 1)
  else
    mersennePrime1 k (p + 1) steps -- (steps - 1)

-- We got some pretty big numbers, for example:
-- mersennePrime1 10 13 2 resulted in:
-- 545374067809707964731492122366891431224132080998116346215916393094860665924559647608132117262600993611978645898078512636554935410088592031805489882538777399539453149421096494769304912614024102579848425806795819098385943271304662280060645276950943150508950126267899958600005039800013267918400452648902940476175250815097737826955502656182280007423713017646775622921964459376384348139672044027808757847174972703338912570407450308052960128219252289006663246782918023621203691221406122565758878759582449613182871861216138684037513813941522603250896380850472849584248628939841925868524998450480560257827525057780635745746257671052874483314773516393160752865414215110832485162198069317625813204758084002713811717998154460845723090593703197655332702442869717416438714083703747685496755934378179985195058510911808374729310484928503131806041353357704078533287568640513511155463782455138379580260439152316205524682284377460483661491229592381713691895136224219009263488882470536357805790217345413729669995980707121370705299558713030278241881878157263805681329314191684310578996819010439268837772668394957847117216977833157535043606767735127835156002065362747917254178719826914468038540489275289456483953676390027467810780545397922586477057986463739938763869280004102059279465002388874363880926906755246920290930799326105802980154178202970910594857018934363109740749363801826808149428087411206516742719392662012375709708591506140539104864651768686402287186047614351811388181972645434903129211177574253785519809693724814933404094384831407889076539696589546571824170380869290909781501497211395377477530644409154215039824346616089579382959017782608078557701496060138077803936553968738733420764181493854349725076015615931297101542846919472328530673118352117013410551479477475598543538273093311398147268225810378254675509453011886910769766388104338489294865983165154446652332584718092539175320784168472265025718745655649417183632619297702452136727964361974762613592308702183927377305237188509884012788302940519038635353858971110988545192719292922047746058049926269451987327851971986543045465298481680383764982469207299092852981877280748677913906811916644453154502144008660712404331981335666764004616379175436529807059361890711050730099307873693427548448044594590220669779262411433770556606319396837783825170181485015965011698914232659273619122116014007594844830209411488000407718805326127135081797825437716925573561607113633302701790890734545403288234475293830998593252832737857896447
--
-- However, unfortunately we appeared to have stumbled upon the smallest Mersenne prime p (8191) for
-- which (2 ^ p) - 1 is composite (the number above): http://www.numbergossip.com/8191.
-- Other attempts were also not very succesful unfortunately.

-- The second thing we tried, was to create a list that would generate mersenne numbers
-- from the provided list of prime numbers (e.g. take 500 primes).
mersenneLst _ [] = return $ []
mersenneLst k (p:primes) = do
  newP <- mersP p
  isP <- primeMR k ((2 ^ p) - 1)
  restPs <- mersenneLst k primes
  if isP then
    return $ (p:restPs)
  else
    return $ restPs

-- Here we found the list (using mersenneLst 3 (take 500 primes)):
-- [2,3,5,7,13,17,19,31,61,89,107,127,521,607,1279,2203,2281,3217]
-- thus, 2 ^ 3217 - 1 should be the highest prime we found after 500 iterations, which is:
-- 259117086013202627776246767922441530941818887553125427303974923161874019266586362086201209516800483406550695241733194177441689509238807017410377709597512042313066624082916353517952311186154862265604547691127595848775610568757931191017711408826252153849035830401185072116424747461823031471398340229288074545677907941037288235820705892351068433882986888616658650280927692080339605869308790500409503709875902119018371991620994002568935113136548829739112656797303241986517250116412703509705427773477972349821676443446668383119322540099648994051790241624056519054483690809616061625743042361721863339415852426431208737266591962061753535748892894599629195183082621860853400937932839420261866586142503251450773096274235376822938649407127700846077124211823080804139298087057504713825264571448379371125032081826126566649084251699453951887789613650248405739378594599444335231188280123660406262468609212150349937584782292237144339628858485938215738821232393687046160677362909315071

-- which is the 18th Mersenne number according to Wikipedia.
-- Further more, the list that was produced is exactly the same as the first 18 of the list on Wikipedia of the first Mersenne primes!

-- Assignment 8 (Bonus)
findFstPrime k (l:lst) = do
  isP <- primeMR k l
  if isP then
    return l
  else
    findFstPrime k lst

genPrime k bits = do
  g <- newStdGen
  prime <- findFstPrime k (randomRs (start, end) g)
  return $ prime
  where start = 2 ^ (bits - 1)
        end = (2 ^ bits) - 1

-- By using genPrime 10 2048 twice, we got two (likely) prime numbers, which can be used as a public and private key.
-- 21830798808544642660247998751359589959067901344794671942994507488690402292342540007056642471445768246465695085986012649758747514157213434360668087533711426969907479838722939000548551273804607623974738023137639148355027082824863798695956054484245831202969959633437227571249001706249394842706159252288197973136476764831058501007404231970101259984435411531783439052070808656865812961431443782354480604004198199526828699777531238059711092004480483095763753688748561134086878594510337627131087719575060205164006335111892326576681198308775678566363098592145902522448326981584861916016805499138889763229130511497401495930341
-- 24651084937395584500681690775806912964663524043855587837559464275309223185746931262355822080602829032825009161237014993451765665503192515679453141445012530266187934292526072632352002355349606921214101631343762053526807621821727023098255438401997056890446728649229932789493793833507540909120064430607600754781719968492173516223880840659784510661063996140838598494887231977619855650287487461141044318898325434164708962362721068215997397522856226020648302973165340665009959987442670886479626596216717162108223043004602263463068141541125000430720660983889984432487023600055489914526612540983223607299402251093719903998371

rsaEq p q sec = rsa_decode pr (rsa_encode pu sec) == sec
  where pu = rsa_public p q
        pr = rsa_private p q

-- The following test shows that for each number < p * q the decoding of the encoded data gives returns the original result.
-- Note that quickCheck prop_encode_decode_same will not work, because the k- and bits-argument
-- will generate also bit-values <= 0.
--
-- The following example will work:
--
-- quickCheck $ prop_encode_decode_same 5 512
--
-- This returned:
--
-- +++ OK, passed 100 tests.
prop_encode_decode_same :: Int -> Int -> Property
prop_encode_decode_same k bits =
  monadicIO $ do p <- run $ genPrime k bits
                 q <- run $ genPrime k bits
                 msg <- run $ randomRIO (0, 2 ^ (bits - 1))
                 -- run $ print $ (show p) ++ " " ++ (show q) ++ " " ++ (show msg)
                 assert $ p == q || rsaEq p q msg

-- However, we are not there yet. In real-life applications, just encrypting a number that is lower than p*q
-- is not sufficient: we want to be able to encrypt and decrypt ALL information.
-- This can be solved by introducing blocks of data, which are passed into the decryptor/encryptor.

-- Therefore we first define two functions, with the following specifications:
-- - intToBlocks (blocksToInt msg b) b == msg
-- - msg is an arbitrary Integer
-- - bls is a list of numbers, for each number < 2 ^ b.
intToBlocks :: Integer -> Int -> [Integer]
intToBlocks 0 _ = []
intToBlocks msg bits = (msg `rem` (2^bits)) : (intToBlocks (msg `quot` (2^bits)) bits)

blocksToInt [] bits = 0
blocksToInt (b:bs) bits = b + (2^bits) * (blocksToInt bs bits)

rsaEncodeBlocks pu msg bits = blocksToInt [ rsa_encode pu x | x <- (intToBlocks msg bits) ] (bits * 2)
rsaDecodeBlocks pu msg bits = blocksToInt [ rsa_encode pu x | x <- (intToBlocks msg (bits * 2)) ] bits


rsaEqBlocks p q sec bits = rsaDecodeBlocks pr (rsaEncodeBlocks pu sec bits) bits == sec
  where pu = rsa_public p q
        pr = rsa_private p q

prop_encode_decode_same_blocks :: Int -> Int -> Property
prop_encode_decode_same_blocks k bits =
  monadicIO $ do p <- run $ genPrime k bits
                 q <- run $ genPrime k bits
                 msg <- run $ randomRIO (0, 2^(bits * 4))
                 run $ print $ (show p) ++ " " ++ (show q) ++ " " ++ (show msg)
                 assert $ p == q || rsaEqBlocks p q msg bits

-- The property can be tested using:

-- quickCheck (prop_encode_decode_same_blocks 5 512)
