import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Time
import Data.Time.Clock.POSIX
import Foreign
import Foreign.C
import System.Win32

#include <windows.h>

eVENTLOG_BACKWARDS_READ  = #{const EVENTLOG_BACKWARDS_READ}
eVENTLOG_SEQUENTIAL_READ = #{const EVENTLOG_SEQUENTIAL_READ}
mAX_RECORD_BUFFER_SIZE = 0x7ffff -- 512K

openEventLog :: String -> String -> IO HANDLE
openEventLog server provider =
  withTString server $ \ c_server ->
  withTString provider $ \ c_provider ->
  failIfNull "OpenEventLog" $ c_OpenEventLogW nullPtr c_provider

foreign import stdcall "windows.h ReadEventLogW" c_ReadEventLogW ::
  HANDLE -> DWORD -> DWORD -> Ptr a -> DWORD -> Ptr DWORD -> Ptr DWORD -> IO Bool -- made PVOID LPVOID

foreign import stdcall "windows.h OpenEventLogW" c_OpenEventLogW ::
  LPCWSTR -> LPCWSTR -> IO HANDLE

data EventLog =
 EventLog { length_             :: DWORD
          , reserved            :: DWORD
          , recordnumber        :: DWORD
          , timegenerated       :: DWORD
          , timewritten         :: DWORD
          , eventid             :: DWORD
          , eventtype           :: WORD
          , numstrings          :: WORD
          , eventcategory       :: WORD
          , reservedflags       :: WORD
          , closingrecordnumber :: DWORD
          , stringoffset        :: DWORD
          , usersidlength       :: DWORD
          , usersidoffset       :: DWORD
          , datalength          :: DWORD
          , dataoffset          :: DWORD
          } deriving Show

instance Storable EventLog where
  sizeOf    _ = #{size EVENTLOGRECORD}
  alignment _ = alignment (undefined :: CInt)
  poke p foo  = do
    #{poke EVENTLOGRECORD, Length              } p $ length_             foo
    #{poke EVENTLOGRECORD, Reserved            } p $ reserved            foo
    #{poke EVENTLOGRECORD, RecordNumber        } p $ recordnumber        foo
    #{poke EVENTLOGRECORD, TimeGenerated       } p $ timegenerated       foo
    #{poke EVENTLOGRECORD, TimeWritten         } p $ timewritten         foo
    #{poke EVENTLOGRECORD, EventID             } p $ eventid             foo
    #{poke EVENTLOGRECORD, EventType           } p $ eventtype           foo
    #{poke EVENTLOGRECORD, NumStrings          } p $ numstrings          foo
    #{poke EVENTLOGRECORD, EventCategory       } p $ eventcategory       foo
    #{poke EVENTLOGRECORD, ReservedFlags       } p $ reservedflags       foo
    #{poke EVENTLOGRECORD, ClosingRecordNumber } p $ closingrecordnumber foo
    #{poke EVENTLOGRECORD, StringOffset        } p $ stringoffset        foo
    #{poke EVENTLOGRECORD, UserSidLength       } p $ usersidlength       foo
    #{poke EVENTLOGRECORD, UserSidOffset       } p $ usersidoffset       foo
    #{poke EVENTLOGRECORD, DataLength          } p $ datalength          foo
    #{poke EVENTLOGRECORD, DataOffset          } p $ dataoffset          foo
  peek p = return EventLog
    `ap` (#{peek EVENTLOGRECORD, Length              } p )
    `ap` (#{peek EVENTLOGRECORD, Reserved            } p )
    `ap` (#{peek EVENTLOGRECORD, RecordNumber        } p )
    `ap` (#{peek EVENTLOGRECORD, TimeGenerated       } p )
    `ap` (#{peek EVENTLOGRECORD, TimeWritten         } p )
    `ap` (#{peek EVENTLOGRECORD, EventID             } p )
    `ap` (#{peek EVENTLOGRECORD, EventType           } p )
    `ap` (#{peek EVENTLOGRECORD, NumStrings          } p )
    `ap` (#{peek EVENTLOGRECORD, EventCategory       } p )
    `ap` (#{peek EVENTLOGRECORD, ReservedFlags       } p )
    `ap` (#{peek EVENTLOGRECORD, ClosingRecordNumber } p )
    `ap` (#{peek EVENTLOGRECORD, StringOffset        } p )
    `ap` (#{peek EVENTLOGRECORD, UserSidLength       } p )
    `ap` (#{peek EVENTLOGRECORD, UserSidOffset       } p )
    `ap` (#{peek EVENTLOGRECORD, DataLength          } p )
    `ap` (#{peek EVENTLOGRECORD, DataOffset          } p )

dwordToLocalTime dtime = utcToLocalTime tz $ posixSecondsToUTCTime $ fromIntegral dtime
  where tz = unsafePerformIO getCurrentTimeZone

main = do
  hEventLog <- openEventLog "localhost" "System"
  let dwBytesToRead = mAX_RECORD_BUFFER_SIZE
  allocaBytes (fromIntegral dwBytesToRead) $ \pBuffer -> do
    alloca $ \ dwBytesRead -> do
    alloca $ \ dwMinimumBytesToRead -> do
      hbool <- c_ReadEventLogW hEventLog
        (eVENTLOG_SEQUENTIAL_READ .|. eVENTLOG_BACKWARDS_READ)
        0
        pBuffer
        dwBytesToRead
        dwBytesRead
        dwMinimumBytesToRead
      errko <- getLastError
      putStrLn ("errko: " ++ show errko)
      events <- peekEvents pBuffer
      mapM_ (print . dwordToLocalTime . timegenerated) events
      putStrLn $ "dwBytesRead           " ++ show dwBytesRead
      putStrLn $ "dwMinimumBytesToRead  " ++ show dwMinimumBytesToRead

peekEvents ptr = go 0 ptr
  where
    go offset ptr = do
         let ptr' = plusPtr ptr (fromIntegral offset)
         e <- peek ptr'
         if length_ e /= 0 then
           do let nextoffset = fromIntegral (length_ e)
              es <- go nextoffset ptr'
              return (e:es)
         else return []
