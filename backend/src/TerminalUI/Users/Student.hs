module TerminalUI.Users.Student
  ( displayStudentOptions,
  )
where

import Lib (displayActionSelectionMessage, selectOption)
import Util.Constants (studentOptionsPrompts)

displayStudentOptions :: [IO ()] -> IO ()
displayStudentOptions actions = do
  displayActionSelectionMessage
  selectOption $ zip studentOptionsPrompts actions