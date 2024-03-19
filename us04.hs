-- Event types
data EventType = Aula | AtividadesExtracurriculares | EventoComum | RegistroDescanso | RegistroLeitura | RegistroCriacao | Trabalho deriving (Show, Eq)

data Visibility = Public | Private deriving (Show, Eq)

data Event = Event {
    eventType :: EventType,
    title :: String,
    visibility :: Visibility,
    frequency :: Int,
    startDate :: String, 
    endDate :: String 
} deriving (Show)

-- Definition of the agenda structure
type Agenda = [Event]

-- Function to create a new event
criarEvento :: EventType -> String -> Visibility -> Int -> String -> String -> Event
criarEvento eType title vis freq start end = Event {
    eventType = eType,
    title = title,
    visibility = vis,
    frequency = freq,
    startDate = start,
    endDate = end
}

-- Function to add an event to the agenda
adicionarEvento :: Event -> Agenda -> Agenda
adicionarEvento event agenda = event : agenda

