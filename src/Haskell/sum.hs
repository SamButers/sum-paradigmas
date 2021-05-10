import System.IO
import System.Environment

import Data.Bits

import qualified Data.ByteString as BS


cyclicShiftR x = ( shiftR x 1 ) + ( shiftL ( (.&.) x 1 ) 15 )

bsdchecksum [] checksum = checksum
bsdchecksum (x:xs) checksum = bsdchecksum xs new_checksum
                            where new_checksum = (.&.) ((cyclicShiftR checksum) + x) 0xffff

main = do
    args <- getArgs

    input <- openFile (args!!0) ReadMode
    byteString <- BS.hGetContents input

    let bytes = map fromIntegral (BS.unpack byteString) :: [Int]

    print $ bsdchecksum bytes 0