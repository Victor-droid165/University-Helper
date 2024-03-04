module Main where

data TipoAnotacao = Lembrete | NotaAdesiva | TextoCorrido | Advertencia
    deriving (Show)

data Anotacao = Anotacao {
    titulo :: String,
    conteudo :: String,
    tipo :: TipoAnotacao,
    visibilidade :: String
} deriving (Show)

-- Função para criar uma nova anotação
criarAnotacao :: String -> String -> TipoAnotacao -> String -> Anotacao
criarAnotacao t c tp v = Anotacao {titulo = t, conteudo = c, tipo = tp, visibilidade = v}

-- Função para imprimir os detalhes de uma anotação
imprimirAnotacao :: Anotacao -> String
imprimirAnotacao (Anotacao t c tp v) = "Título: " ++ t ++ "\nConteúdo: " ++ c ++ "\nTipo: " ++ show tp ++ "\nVisibilidade: " ++ v

-- Função principal
main :: IO ()
main = do
    let anotacao1 = criarAnotacao "Lembrete de Prova" "Prova de Matemática na sexta-feira" Lembrete "Público"
    --
    putStrLn "Detalhes das Anotações:"
    putStrLn $ imprimirAnotacao anotacao1
