-- Tidak perlu diaktifkan, sudah tersedia di Prelude (built-in library)
-- data Maybe a = Nothing
--             | Just a
-- lookup :: a -> [(a, b)] -> Maybe b

import Data.List



animalFriends :: [(String, String)]
animalFriends = [ ("Pony", "Lion")
                , ("Lion", "Manticore")
                , ("Manticore", "Unicorn")
                , ("Unicorn", "Lepricon") ]

                ￼Live Coding in ClassURL
                ￼
                Dosen Tamu, HaLeX dan Parser Combinator
                ￼
                Higher Order Function
                ￼Latihan Higher Order FunctionFile
                ￼Latihan di Kelas, Trees.lhs (Simple Arithmatics with variable)File
                ￼
                Partial Evaluation
                ￼Review Essay Bulan Pertama (kerjakan latihan terlebih dahulu)Assignment
                ￼
                Lazy Evaluation
                ￼Catatan dan Latihan Lazy EvaluationFile
                ￼Contoh Jawaban Latihan - - silahkan tambahkan contoh/solusi alternatifURL
                ￼
                Monad, Higher Order Type
                ￼Slide Monad from Phillip MatesFile
                ￼Contoh Monad di KelasFile
                ￼Berkas Robot.lhs yang sudah diperbaharuiFile
                ￼
                Kuis dan Diskusi, Latihan
                Untuk membantu mencicil dan mempersiapkan ujian lebih baik, kita jadwalkan kuis pada waktu berikut ini.
                
                Kelas A Senin sore, tgl 14 Okt jam 16:00 - 17:40 di gedung baru ruang Auditorium
                Kelas B Selasa pagi, tgl 15 Okt jam 8:00 -  9:40 di gedung lama ruang 2402
                Kuis ini akan dilakukan 75 menit, dimulai 15 menit dari jadwal kuliah seharusnya. Soal terdiri dari 5-10 soal, peserta kuliah tidak harus mengerjakan seluruhnya. Cukup mengerjakan setengah nya saja. Silahkan pilih soal yang sudah sempat dilatih dan dipahami. Saat ujian nanti jumlah dan variasi tingkat kesulitan relatif sama, namun peserta diminta untuk mengerjakan semuanya dalam waktu 2.5 jam.
                
                Kuis dan ujian dilakukan secara tutup buku. Sebagian besar soal akan diambil relatif sama persis dengan soal latihan atau contoh yang dibahas di kelas. Silahkan berlatih dan pahami dengan baik. Peserta tidak diharapkan menghafal, melainkan memahaminya dengan baik sehingga bisa me-rekonstruksi kembali.
                
                Selamat berlatih.
                
                salam,
                
                Ade Azurat
                
                ￼Cover Page Kuis - Harap Baca terlebih dahulu Aturan KuisFile
                ￼
                Mid-Test (Closed Book)
                ￼Learning Objective (Acuan checklist pemelajaran, buat commit di repo latihan masing2)Page
                ￼Submit informasi URL git repository latihan masing2. Jadikan public atau berikan hak akses ke adeazurat@gmail.comAssignment
                ￼Informasi commit latihan terkait soal UTSQuestionnaire
                ￼Latihan Tambahan Persiapan UTS (lakukan commit secara berkala walau belum berhasil)File
                ￼Cover Page UTS (hadir 15 menit sebelum ujian, yg telat tidak boleh masuk ruang ujian. Ketentuan Ujian, harap dipahami terlebih dahulu)File
                ￼
                Topic 9
                ￼
                Topic 10
                ￼
                Topic 11
                ￼
                Topic 12
                ￼
                Topic 13
                ￼
                Topic 14
                ￼
                Topic 15
                ￼
                Topic 16
                Skip Clock
                ￼￼Clock
                ￼	Server:	￼
                Skip Calendar
                ￼￼Calendar
                ◄ Previous monthOctober 2019Next month ►
                Sun	Mon	Tue	Wed	Thu	Fri	Sat
                      1	2	
                3
                4	5
                6	7	8	9	10	11	12
                13	14	15	16	
                17
                18	Today Saturday, 19 October
                19
                20	21	22	23	24	25	26
                27	28	29	30	31	 	 
                EVENTS KEY
                ￼Hide global events
                ￼Hide course events
                ￼Hide group events
                ￼Hide user events
                Skip Navigation
                ￼Navigation
                Home
                
                ￼Dashboard
                
                My courses
                
                PROG. IK REGULAR
                
                REG - Genap 2017/2018
                
                REG - Genap 2018/2019
                
                REG - Gasal 2019/2020
                
                ￼[Reg] Jaringan Komputer (A,B)
                
                ￼[Reg & KI] Pemrograman Sistem Gasal 2019/2020
                
                [Reg] Pemrograman Fungsional (A,B)
                
                Participants
                
                ￼Badges
                
                ￼Competencies
                
                ￼Grades
                
                ￼[Reg] Pengolahan Citra (A) - Gasal 2019/2020
                
                REG - Gasal 2018/2019
                
                PROG. S1 FAK. REGULER
                
                ￼More...
                
                Skip Administration
                ￼￼Administration
                Course administration
                
                ￼Unenrol me from Pemrog Fungsional-CSCE604123-Gasal1920
                
                
-- Does Pony's friend have a friend in animalMap?
animalFriendLookup :: [(String, String)] -> Maybe String
animalFriendLookup animalMap =
  case lookup "Pony" animalMap of
       Nothing -> Nothing
       Just ponyFriend ->
         case lookup ponyFriend animalMap of
              Nothing -> Nothing
              Just ponyFriendFriend -> 
                case lookup ponyFriendFriend animalMap of
                  Nothing -> Nothing
                  Just ponyFriendFriendFriend -> 
                    case lookup ponyFriendFriendFriend animalMap of
                        Nothing -> Nothing
                        Just friend -> Just friend

monadicFriendLookup :: [(String, String)] -> Maybe String
monadicFriendLookup animalMap =
  lookup "Pony" animalMap
  >>= (\ponyFriend -> lookup ponyFriend animalMap
  >>= (\pony2ndFriend -> lookup pony2ndFriend animalMap
  >>= (\friend -> Just friend)))

sugaryFriendLookup :: [(String, String)] -> Maybe String
sugaryFriendLookup animalMap = do
  ponyFriend    <- lookup "Pony" animalMap
  ponyFriend'   <- lookup ponyFriend animalMap
  ponyFriend''  <- lookup ponyFriend' animalMap
  return ponyFriend''

test0 = [(x,y)| x<-[1,2,3],y<-[4,5]]

test1 = do
        x <- [1,2,3]
        y <- [4,5]
        return (x,y)

test2 = ([1,2,3] >>= \x -> [4,5] >>= \y -> return (x,y))

-- instance  Monad []  where	    
--    m >>= k	=  concat (map k m)	    
--    return x	=  [x]	    
--    fail x	=  [ ]

test3 = ([1,2,3] >>= (\x -> [4,5] >>= (\y -> return (x,y))))

-- tahap 1
test_1 = concat (map (\x -> [4,5] >>= (\y -> return (x,y))) 
                    [1,2,3])

-- tahap 2
-- [4,5] >>= (\y -> return (x,y))
-- concat (map (\y -> return (x,y) [4,5] )
test_2 = concat (map (\x -> concat (map (\y -> return (x,y)) [4,5]))
                     [1,2,3])

-- tahap 3
-- return (x,y) = [(x,y)]
test_3 = concat (map (\x -> concat (map (\y -> [(x,y)]) [4,5] )  ) 
                     [1,2,3])

test4 = concat (map (\x -> (concat (map (\y -> [(x,y)])
                                        [4,5])))
                    [1,2,3])

type Sexpr = String

-- naive generation of unique symbol
transformStmt :: Sexpr -> Int -> (Sexpr, Int)
transformStmt expr counter = (newExpr, counter+1)
  where newExpr = "(define " ++ var ++ " " ++ expr ++ ")"
        var = "tmpVar" ++ (show counter)

primas = sieve [2..]
  where sieve (x:xs) = x : sieve [y | y<-xs, y `mod` x /=0]

satu = 1 : satu