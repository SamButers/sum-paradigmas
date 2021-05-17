import System.Environment
import System.Exit
import System.IO
import System.Directory

import Data.Bits

import qualified Data.ByteString as BS

cyclicShiftR :: Int -> Int
cyclicShiftR x = (shiftR x 1) + (shiftL ((.&.) x 1) 15)

bsdchecksum :: [Int] -> Int -> Int
bsdchecksum [] checksum = checksum
bsdchecksum (x:xs) checksum = bsdchecksum xs new_checksum
                            where new_checksum = (.&.) ((cyclicShiftR checksum) + x) 0xffff

sysvchecksum :: [Int] -> Int
sysvchecksum bytes = ((.&.) r 0xffff) + (shiftR r 16)
                     where
                        s = sum bytes
                        r = ((.&.) s 0xffff) + (shiftR ((.&.) s 0xffffffff) 16)

formatInvalidFlag :: [Char] -> [Char]
formatInvalidFlag ('-' : '-' : flag) = "--" ++ flag
formatInvalidFlag ('-' : flag) = flag

strMul :: [Char] -> Int -> [Char]
strMul str multiplier = concat $ replicate multiplier str

formatChecksum :: Int -> [Char]
formatChecksum checksum = (strMul "0" (5 - checksumSize)) ++ checksumString
                        where
                            checksumString = show checksum
                            checksumSize = length checksumString

printInvalidOption :: [Char] -> IO ()
printInvalidOption ('-' : flag) = putStrLn ("sum: unrecognized option '-" ++ flag ++ "'\n\
                                            \Try 'sum --help' for more information.")
printInvalidOption flag = putStrLn ("sum: invalid option -- '" ++ [flag!!0] ++ "'\n\
                                    \Try 'sum --help' for more information.")

getBlocks :: Handle -> [Char] -> IO Integer
getBlocks handle "-s" = do
    size <- hFileSize handle
    return $ ceiling ((fromIntegral size) / 512)
getBlocks handle "-r" = do
    size <- hFileSize handle
    return $ ceiling ((fromIntegral size) / 1024)

getFlag :: [[Char]] -> [Char] -> IO ([Char])
getFlag [] flag = return flag
getFlag ("-r":fs) flag = getFlag fs "-r"
getFlag ("-s":fs) flag = getFlag fs "-s"
getFlag ("--sysv":fs) flag = getFlag fs "-s"
getFlag ("--version":fs) flag = do
    putStrLn "sum (GNU coreutils) 8.30\n\
    \Copyright (C) 2018 Free Software Foundation, Inc.\n\
    \License GPLv3+: GNU GPL version 3 or later <https://gnu.org/licenses/gpl.html>.\n\
    \This is free software: you are free to change and redistribute it.\n\
    \There is NO WARRANTY, to the extent permitted by law.\n\
    \\n\
    \Written by Kayvan Aghaiepour and David MacKenzie."
    exitSuccess
getFlag ("--help":fs) flag = do
    putStrLn "Usage: sum [OPTION]... [FILE]...\n\
    \Print checksum and block counts for each FILE.\n\
    \\n\
    \With no FILE, or when FILE is -, read standard input.\n\
    \\n\
    \  -r              use BSD sum algorithm, use 1K blocks\n\
    \  -s, --sysv      use System V sum algorithm, use 512 bytes blocks\n\
    \      --help     display this help and exit\n\
    \      --version  output version information and exit\n\
    \\n\
    \GNU coreutils online help: <https://www.gnu.org/software/coreutils/>\n\
    \Full documentation at: <https://www.gnu.org/software/coreutils/sum>\n\
    \or available locally via: info '(coreutils) sum invocation'"
    exitSuccess
getFlag (f:fs) flag = do
    let formattedFlag = formatInvalidFlag f
    printInvalidOption formattedFlag
    exitSuccess

isStdinTarget :: [[Char]] -> [[Char]]
isStdinTarget [] = [[], "stdin"]
isStdinTarget targets = targets

printBSDChecksums :: [[Char]] -> Int -> IO ()
printBSDChecksums [] sums = return ()
printBSDChecksums ([]:["stdin"]) sums = do
    inputByteString <- BS.getContents
    let inputBytes = map fromIntegral (BS.unpack inputByteString) :: [Int]
    let blocks = show $ ceiling $ (fromIntegral (length inputBytes))/1024

    putStrLn ((formatChecksum $ bsdchecksum inputBytes 0) ++ (strMul " " (max 1 (6 - (length blocks)))) ++ blocks)
printBSDChecksums (target:targets) sums = do
    isValidFile <- doesFileExist target
    if isValidFile
        then do
            input <- openFile target ReadMode

            blocks <- getBlocks input "-r"
            let blocksStr = show blocks

            inputByteString <- BS.hGetContents input
            let inputBytes = map fromIntegral (BS.unpack inputByteString) :: [Int]

            if sums > 1
                then putStrLn ((formatChecksum $ bsdchecksum inputBytes 0) ++ (strMul " " (max 1 (6 - (length blocksStr)))) ++ blocksStr ++ " " ++ target)
                else putStrLn ((formatChecksum $ bsdchecksum inputBytes 0) ++ (strMul " " (max 1 (6 - (length blocksStr)))) ++ blocksStr)

            hClose input
        else do
            putStrLn ("sum: " ++ target ++ ": No such file or directory")

    printBSDChecksums targets sums

printChecksums :: [[Char]] -> [Char] -> IO ()
printChecksums [] flag = return ()
printChecksums targets "-r" = printBSDChecksums targets $ length targets
printChecksums ([]:["stdin"]) "-s" = do
    inputByteString <- BS.getContents
    let inputBytes = map fromIntegral (BS.unpack inputByteString) :: [Int]
    let blocks = show $ ceiling $ (fromIntegral (length inputBytes))/512

    putStrLn ((show $ sysvchecksum inputBytes) ++ " " ++ blocks)
printChecksums (target:targets) "-s" = do
    isValidFile <- doesFileExist target
    if isValidFile
        then do
            input <- openFile target ReadMode

            blocks <- getBlocks input "-s"
            let blocksStr = show blocks

            inputByteString <- BS.hGetContents input
            let inputBytes = map fromIntegral (BS.unpack inputByteString) :: [Int]

            putStrLn ((show $ sysvchecksum inputBytes) ++ " " ++ blocksStr ++ " " ++ target)

            hClose input
        else do
            putStrLn ("sum: " ++ target ++ ": No such file or directory")

    printChecksums targets "-s"

main = do
    args <- getArgs

    let flags = filter (\x -> (x!!0) == '-') args
    let targets = isStdinTarget $ filter (\x -> (x!!0) /= '-') args

    flag <- getFlag flags "-r"

    printChecksums targets flag
