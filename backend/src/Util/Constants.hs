module Util.Constants
  ( teacherOptionsPrompts,
    studentOptionsPrompts,
    administratorOptionsPrompts,
    emailInputPrompts,
    passwordInputPrompt,
    userNameInputPrompts,
    universityInputPrompts,
    userEnrollmentInputPrompts,
  )
where

teacherOptionsPrompts :: [String]
teacherOptionsPrompts =
  [ "REMOVER SUA CONTA",
    "SAIR"
  ]

studentOptionsPrompts :: [String]
studentOptionsPrompts =
  [ "REMOVER SUA CONTA",
    "SAIR"
  ]

administratorOptionsPrompts :: [String]
administratorOptionsPrompts =
  [ "CADASTRAR USUARIO",
    "REMOVER USUARIO DO SISTEMA",
    "ATUALIZAR INFORMACOES DE UM USUARIO",
    "VALIDAR CADASTROS",
    "SAIR"
  ]

emailInputPrompts :: [String]
emailInputPrompts = ["Agora informe-nos o e-mail do usuario", "Digite o E-MAIL da pessoa que utilizara o sistema: "]

passwordInputPrompt :: String
passwordInputPrompt = "Digite a SENHA que a pessoa utilizara para o login: "

userNameInputPrompts :: [String]
userNameInputPrompts =
  [ "Agora precisamos saber qual o nome do usuario que você ira cadastrar",
    "Digite o NOME da pessoa que usara o sistema: "
  ]

universityInputPrompts :: [String]
universityInputPrompts =
  [ "A qual universidade o usuario faz parte?",
    "Digite o NOME da universidade que constara no sistema: "
  ]

userEnrollmentInputPrompts :: [String]
userEnrollmentInputPrompts =
  [ "Agora precisamos saber qual a matricula do usuario",
    "Digite o numero de MATRICULA da pessoa que usara o sistema: "
  ]
