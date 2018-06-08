
-- MATHFUN
--
-- Student ID: 782738

-- Types

import Data.List

type Title = String
type Director = String
type Year = Int
type Likes = String
type Dislikes = String

data Film = Film Title Director Year [Likes] [Dislikes]
            deriving(Eq,Ord,Show,Read)

-- Film format (Film "" "" Int [] [])

testDatabase :: [Film]
testDatabase = [
    (Film "Blade Runner" "Ridley Scott" 1982 ["Zoe", "Heidi", "Jo", "Kate", "Emma", "Liz", "Dave"] ["Sam", "Olga", "Tim"]),
    (Film "The Fly" "David Cronenberg" 1986 ["Garry", "Dave", "Zoe"] ["Kevin", "Emma", "Heidi", "Jo", "Kate"]),
    (Film "Body Of Lies" "Ridley Scott" 2008 ["Garry", "Dave"] ["Bill", "Olga", "Tim", "Zoe", "Paula"]),
    (Film "Avatar" "James Cameron" 2009 ["Dave", "Amy", "Liz"] ["Olga", "Tim", "Zoe", "Paula"]),
    (Film "Titanic" "James Cameron" 1997 ["Zoe", "Emma", "Paula", "Liz", "Olga", "Dave"] ["Sam", "Wally", "Kate"]),
    (Film "The Departed" "Martin Scorsese" 2006 ["Wally", "Liz", "Kevin", "Tim", "Emma"] ["Olga", "Dave", "Kate", "Zoe"]),
    (Film "Aliens" "Ridley Scott" 1986 ["Dave", "Garry", "Liz", "Sam", "Wally", "Kate", "Zoe"] ["Tim", "Emma", "Jo", "Olga"]),
    (Film "Kingdom Of Heaven" "Ridley Scott" 2005 ["Jo", "Wally", "Emma"] ["Tim", "Garry", "Ian", "Neal"]),
    (Film "Alien: Covenant" "Ridley Scott" 2017 ["Kevin", "Tim"] ["Emma", "Jo", "Liz"]),
    (Film "E.T. The Extra-Terrestrial" "Steven Spielberg" 1982 ["Dave", "Amy", "Garry", "Ian", "Neal"] ["Jenny", "Kate", "Emma", "Olga"]),
    (Film "Bridge of Spies" "Steven Spielberg" 2015 ["Wally", "Sam", "Dave", "Neal"] ["Bill", "Garry", "Ian", "Kate"]),
    (Film "Jaws" "Steven Spielberg" 1975 ["Jenny", "Emma", "Bill", "Neal"] ["Sam", "Ian", "Kate"]),
    (Film "The Martian" "Ridley Scott" 2015 ["Wally", "Sam", "Dave", "Jo", "Jenny", "Kate", "Emma", "Olga"] ["Ian", "Neal", "Tim", "Liz"]),
    (Film "The BFG" "Steven Spielberg" 2016 ["Sam", "Wally", "Dave", "Jo", "Kate"] ["Neal"]),
    (Film "The Shawshank Redemption" "Frank Darabont" 1994 ["Dave", "Amy", "Bill", "Garry", "Ian", "Neal", "Kate", "Jenny", "Zoe", "Heidi"] ["Jo"]),
    (Film "Gladiator" "Ridley Scott" 2000 ["Olga", "Neal", "Kate", "Garry"] ["Heidi", "Bill", "Sam", "Zoe"]),
    (Film "The Green Mile" "Frank Darabont" 1999 ["Kevin", "Tim", "Emma", "Heidi"] ["Kate", "Jenny", "Zoe"]),
    (Film "True Lies" "James Cameron" 1994 ["Sam", "Dave"] ["Emma", "Olga", "Jenny", "Zoe"]),
    (Film "Super 8" "J J Abrams" 2011 ["Kevin", "Tim", "Emma", "Olga", "Heidi"] ["Wally", "Dave", "Jenny", "Zoe"]),
    (Film "Minority Report" "Steven Spielberg" 2002 ["Kevin", "Kate", "Tim", "Emma", "Jenny", "Zoe"] ["Olga", "Heidi"]),
    (Film "War Horse" "Steven Spielberg" 2011 ["Garry", "Bill", "Olga", "Jo", "Wally", "Emma", "Tim", "Kate", "Zoe"] ["Heidi", "Jenny", "Sam"]),
    (Film "Silence" "Martin Scorsese" 2016 ["Wally", "Emma", "Tim", "Heidi", "Bill", "Jo"] ["Dave", "Olga"]),
    (Film "The Terminal" "Steven Spielberg" 2004 ["Kate", "Dave", "Jo", "Wally", "Emma"] ["Heidi"]),
    (Film "Star Wars: The Force Awakens" "J J Abrams" 2015 ["Emma", "Wally", "Zoe", "Kate", "Bill", "Dave", "Liz"] ["Olga", "Jo", "Neal"]),
    (Film "Hugo" "Martin Scorsese" 2011 ["Wally", "Sam"] ["Kate", "Bill", "Dave"])
 ]

--  Functional code

--(i)

addFilm :: Film -> [Film] -> [Film]
addFilm f db = f : db

--(ii)

showAllFilms :: [Film] -> [Film]
showAllFilms db = db

--(iii)

showDirectorFilms director = filter(\(Film _ dir _ _ _ ) -> dir == director)

--(iv)

countLikesFilm :: Title -> [Film] -> Float
countLikesFilm _ [] = 0
countLikesFilm t ((Film title _ _ likes _):xs)
    | t == title    = fromIntegral (length likes)
    | otherwise     = countLikesFilm t xs

countDislikesFilm :: Title -> [Film] -> Float
countDislikesFilm _ [] = 0
countDislikesFilm t ((Film title _ _ _ dislikes):xs)
    | t == title    = fromIntegral (length dislikes)
    | otherwise     = countDislikesFilm t xs

giveFilmRating :: Title -> [Film] -> Float
giveFilmRating _ [] = 0
giveFilmRating t ((Film title director year likes dislikes):xs)
    | t == title = filmRating
    | otherwise = giveFilmRating t xs
    where filmRating = (((countLikesFilm title ((Film title director year likes dislikes):xs)) / ((countLikesFilm title ((Film title director year likes dislikes):xs)) + (countDislikesFilm title ((Film title director year likes dislikes):xs)))) * 100)

highRatedFilms :: [Film] -> [Film]
highRatedFilms [] = []
highRatedFilms ((Film title director year likes dislikes):xs)
    | giveFilmRating title ((Film title director year likes dislikes):xs) >= 75 = (Film title director year likes dislikes) : highRatedFilms xs
    | otherwise = highRatedFilms xs

--(v)

totalFilmRating :: Director -> [Film] -> Float
totalFilmRating _ [] = 0
totalFilmRating d ((Film title director year likes dislikes):xs)
    | d == director = filmRating + totalFilmRating d xs
    | otherwise     = totalFilmRating d xs
    where filmRating = (((countLikesFilm title ((Film title director year likes dislikes):xs)) / ((countLikesFilm title ((Film title director year likes dislikes):xs)) + (countDislikesFilm title ((Film title director year likes dislikes):xs)))) * 100)


averageRating :: Director -> [Film] -> Float
averageRating d db = (totalFilmRating d db) / fromIntegral(length (showDirectorFilms d db))

--(vi)


userRated :: String -> [Film] -> [Title]
userRated _ [] = []
userRated user ((Film title director year likes dislikes):xs)
    | userAlreadyLikes user likes           = ("Likes : " ++ title ++ " ") : userRated user xs
    | userAlreadyDislikes user dislikes     = ("Dislikes : " ++ title ++ " ") : userRated user xs
    | otherwise                             = userRated user xs

--(vii)

userAlreadyLikes :: Likes -> [Likes] -> Bool
userAlreadyLikes _ [] = False
userAlreadyLikes like (x:xs)
    | like == x     = True
    | otherwise     = userAlreadyLikes like xs

userAlreadyDislikes :: Dislikes -> [Dislikes] -> Bool
userAlreadyDislikes _ [] = False
userAlreadyDislikes dislike (x:xs)
    | dislike == x  = True
    | otherwise     = userAlreadyDislikes dislike xs 

addLike :: Likes -> Title -> [Film] -> [Film]
addLike _ _ [] = []
addLike like t ((Film title director year likes dislikes):xs)
    | not(userAlreadyLikes like likes) && t == title    =((Film title director year (like:likes) dislikes):xs)
    | otherwise                                         = (Film title director year likes dislikes) : addLike like t xs

addDislike :: Dislikes -> Title -> [Film] -> [Film]
addDislike _ _ [] = []
addDislike dislike t ((Film title director year likes dislikes):xs)
    | not(userAlreadyDislikes dislike dislikes) && t == title   = ((Film title director year likes (dislike:dislikes)):xs)
    | otherwise                                                 = (Film title director year likes dislikes) : addDislike dislike t xs


-- Demo function to test basic functionality (without persistence - i.e. 
-- testDatabase doesn't change and nothing is saved/loaded to/from file).

demo :: Int -> IO()
demo 1  = do putStrLn "All films after adding 2018 film Sherlock Gnomes directed by by John Stevenson to testDatabase";
             showFilms (addFilm (Film "Sherlock Gnomes" "John Stevenson" 2018 [] []) testDatabase);
demo 2  = do putStrLn "(filmsAsString testDatabase)";
             showFilms testDatabase;
demo 3  = do putStrLn "All films by Ridley Scott";
             showFilms ( showDirectorFilms "Ridley Scott" testDatabase ); 
demo 4  = do putStrLn "All films with website rating >= 75%";
             showFilms ( highRatedFilms testDatabase );             
demo 5  = do putStrLn "Average website rating for Ridley Scott";
             putStrLn ("The average website rating for Ridley Scott is : " ++ show (averageRating "Ridley Scott" testDatabase));             
demo 6  = do putStrLn "titles of films rated by Emma (with likes/dislikes)";
             showRated (userRated "Emma" testDatabase);
demo 7  = do putStrLn "All films after Emma says she likes Avatar";
             showFilms (addLike "Emma" "Avatar" testDatabase);
demo 71 = do putStrLn "All films after Emma says she likes Titanic";
             showFilms (addLike "Emma" "Titanic" testDatabase);
demo 72 = do putStrLn "All films after Emma says she dislikes Jaws";
             showFilms (addDislike "Emma" "Titanic" testDatabase);

--Your user interface code goes here

main :: IO()
main = do dbtext <- readFile "filmsdatabase.txt";
          films <- return (read dbtext :: [Film]);
          putStrLn "Please enter your name: ";
          name <- getLine;
          putStrLn "";
          putStrLn ("Hello: " ++ name);
          putStrLn "";
          menu name films;
          
menu :: String -> [Film] -> IO()
menu name db = do putStrLn "";
                  putStrLn "1 -> Add a film";
                  putStrLn "2 -> Display all films";
                  putStrLn "3 -> Display all films from a given director";
                  putStrLn "4 -> Display films with a rating over 75%";
                  putStrLn "5 -> Display average director rating";
                  putStrLn "6 -> Give the titles that a particular user has rated";
                  putStrLn "7 -> Add like to given film";
                  putStrLn "8 -> Add dislike to given film";
                  putStrLn "9 -> Exit";
                  putStrLn "Please enter a number: ";
                  number <- inputNum;
                  putStrLn "";
                  putStrLn "";
                  input number name db;
                  
input :: Int -> String -> [Film] -> IO()
input 1 name db = do film <- inputFilm;
                     menu name (addFilm film db);
input 2 name db = do showFilms db;
                     menu name db;
input 3 name db = do putStrLn "Enter the director name: ";
                     director <- getLine;
                     putStrLn "";
                     showFilms (showDirectorFilms director db);
                     menu name db;
input 4 name db = do showFilms (highRatedFilms db);
                     menu name db;
input 5 name db = do putStrLn "Enter the director name: ";
                     director <- getLine;
                     putStrLn "";
                     putStrLn ("The average website rating for " ++ director ++ " is: " ++ show (averageRating director db));
                     menu name db;
input 6 name db = do putStrLn "Enter user name: ";
                     user <- getLine;
                     showRated (userRated user db);
                     menu name db;
input 7 name db = do putStrLn "Enter movie title: ";
                     title <- getLine;
                     showFilms (addLike name title db);
                     menu name db;
input 8 name db = do putStrLn "Enter movie title: ";
                     title <- getLine;
                     showFilms (addDislike name title db);
                     menu name db;
input 9 _ db   = do putStrLn "End";
                     writeFile "filmsdatabase.txt" (show db);
             
showFilm :: Film -> IO()
showFilm (Film title director year likes dislikes) = do putStrLn title;
                                                        putStrLn ("Director: " ++ director);
                                                        putStrLn ("Year: " ++ show year);
                                                        putStrLn ("Likes: " ++ intercalate " " likes);
                                                        putStrLn ("Dislikes: " ++ intercalate " " dislikes ++ "\n");
                                                                                                               
showFilms :: [Film] -> IO()
showFilms []        = putStrLn "";
showFilms (x:xs)    = do showFilm x;
                         showFilms xs;

showRate :: Title -> IO()
showRate t = do putStrLn (t ++ "\n");
                         
showRated :: [Title] -> IO()
showRated []        = putStrLn "";
showRated (x:xs)    = do showRate x;
                         showRated xs;
                         
inputNum :: IO Int
inputNum = do num <- getLine
              return (read num :: Int)
                         
inputFilm :: IO Film
inputFilm = do putStrLn "What is the film title? ";
               title <- getLine;
               putStrLn "Who is the film director? ";
               director <- getLine;
               putStrLn "What year was the film released? ";
               year <- inputNum;
               return (Film title director year [] []);