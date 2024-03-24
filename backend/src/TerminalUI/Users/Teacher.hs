module TerminalUI.Users.Teacher
  ( displayTeacherOptions,
  )
where

import Lib (displayActionSelectionMessage, selectOption)
import Util.Constants (teacherOptionsPrompts)

displayTeacherOptions :: [IO ()] -> IO ()
displayTeacherOptions actions = do
  displayActionSelectionMessage
  selectOption $ zip teacherOptionsPrompts actions