import Data.Time.Clock
import Data.List
import System.IO
import Data.Maybe (isNothing)
import Control.Exception
import Control.DeepSeq (deepseq)
import System.Directory (doesFileExist)
import Data.Maybe (isNothing)  -- Añade esta línea

-- Definición del tipo de datos para representar la información de un estudiante
data Estudiante = Estudiante {
    idEstudiante :: String,
    nombreEstudiante :: String,
    entrada :: UTCTime,
    salida :: Maybe UTCTime  -- Usamos Maybe para representar que el estudiante aún está en la universidad o ya salió
} deriving (Show, Read)

-- Función para registrar la entrada de un estudiante a la universidad
registrarEntrada :: String -> String -> UTCTime -> [Estudiante] -> Either String [Estudiante]
registrarEntrada id nombre tiempo universidad =
    case buscarEstudianteActivo id universidad of
        Just _ -> Left $ "Error: Ya existe un estudiante con ID " ++ id ++ " dentro de la universidad."
        Nothing -> Right $ Estudiante id nombre tiempo Nothing : universidad

-- Función para registrar la salida de un estudiante de la universidad
registrarSalida :: String -> UTCTime -> [Estudiante] -> Either String [Estudiante]
registrarSalida id tiempo universidad =
    case buscarEstudianteActivo id universidad of
        Just _ -> Right $ map (\e -> if id == idEstudiante e && isNothing (salida e) 
                                    then e { salida = Just tiempo } 
                                    else e) universidad
        Nothing -> Left $ "Error: No se encontró un estudiante activo con ID " ++ id ++ " en la universidad."

-- Función para buscar un estudiante activo por su ID en la universidad
buscarEstudianteActivo :: String -> [Estudiante] -> Maybe Estudiante
buscarEstudianteActivo id universidad =
    find (\e -> id == idEstudiante e && isNothing (salida e)) universidad

-- Función para buscar un estudiante por su nombre en la universidad
buscarEstudiantePorNombre :: String -> [Estudiante] -> [Estudiante]
buscarEstudiantePorNombre nombre universidad =
    filter (\e -> nombre == nombreEstudiante e && isNothing (salida e)) universidad

-- Función para calcular el tiempo que un estudiante ha permanecido en la universidad
tiempoEnUniversidad :: Estudiante -> UTCTime -> NominalDiffTime
tiempoEnUniversidad estudiante tiempoActual =
    case salida estudiante of
        Just tiempoSalida -> diffUTCTime tiempoSalida (entrada estudiante)
        Nothing -> diffUTCTime tiempoActual (entrada estudiante)

-- Función para formatear el tiempo en horas, minutos y segundos
formatearTiempo :: NominalDiffTime -> String
formatearTiempo tiempo =
    let totalSegundos = floor tiempo :: Integer
        horas = totalSegundos `div` 3600
        minutos = (totalSegundos `mod` 3600) `div` 60
        segundos = totalSegundos `mod` 60
    in show horas ++ " horas, " ++ show minutos ++ " minutos, " ++ show segundos ++ " segundos"

-- Función para guardar la información de los estudiantes en un archivo de texto
guardarUniversidad :: [Estudiante] -> IO ()
guardarUniversidad universidad = do
    withFile "University.txt" WriteMode $ \h -> do
        hPutStr h (unlines (map mostrarEstudiante universidad))
    putStrLn "Cambios guardados en el archivo University.txt."

-- Función para cargar la información de los estudiantes desde un archivo de texto
cargarUniversidad :: IO [Estudiante]
cargarUniversidad = do
    existe <- doesFileExist "University.txt"
    if existe
        then do
            contenido <- withFile "University.txt" ReadMode $ \h -> do
                contenido <- hGetContents h
                contenido `deepseq` return contenido
            let lineas = lines contenido
            if null lineas
                then return []
                else return (map leerEstudiante lineas)
        else do
            putStrLn "El archivo University.txt no existe. Se creará uno nuevo."
            return []
    where
        leerEstudiante linea = read linea :: Estudiante

-- Función para mostrar la información de un estudiante como cadena de texto
mostrarEstudiante :: Estudiante -> String
mostrarEstudiante (Estudiante id nombre entrada salida) =
    "Estudiante {idEstudiante = \"" ++ id ++ "\", nombreEstudiante = \"" ++ nombre ++ "\", entrada = " ++ show entrada ++ ", salida = " ++ maybe "Nothing" show salida ++ "}"

-- Función para listar los estudiantes en la universidad
listarEstudiantes :: [Estudiante] -> IO ()
listarEstudiantes [] = putStrLn "No hay estudiantes en la universidad."
listarEstudiantes estudiantes = do
    tiempoActual <- getCurrentTime
    putStrLn "Estudiantes en la universidad:"
    mapM_ (\e -> putStrLn $ mostrarInfoEstudiante e tiempoActual) estudiantes

-- Función para mostrar información detallada de un estudiante
mostrarInfoEstudiante :: Estudiante -> UTCTime -> String
mostrarInfoEstudiante estudiante tiempoActual =
    "ID: " ++ idEstudiante estudiante ++ 
    ", Nombre: " ++ nombreEstudiante estudiante ++ 
    ", Entrada: " ++ show (entrada estudiante) ++ 
    ", Salida: " ++ maybe "Aún en la universidad" show (salida estudiante) ++
    ", Tiempo: " ++ formatearTiempo (tiempoEnUniversidad estudiante tiempoActual)

-- Función principal del programa
main :: IO ()
main = do
    -- Cargar la universidad desde el archivo de texto
    universidad <- cargarUniversidad
    putStrLn "¡Bienvenido al Sistema de Gestión de Estudiantes Universitarios!"

    -- Ciclo principal del programa
    cicloPrincipal universidad

-- Función para el ciclo principal del programa
cicloPrincipal :: [Estudiante] -> IO ()
cicloPrincipal universidad = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de estudiante"
    putStrLn "2. Registrar salida de estudiante"
    putStrLn "3. Buscar estudiante por ID"
    putStrLn "4. Buscar estudiante por nombre"
    putStrLn "5. Listar estudiantes"
    putStrLn "6. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el ID del estudiante:"
            id <- getLine
            putStrLn "Ingrese el nombre del estudiante:"
            nombre <- getLine
            tiempoActual <- getCurrentTime
            case registrarEntrada id nombre tiempoActual universidad of
                Right universidadActualizada -> do
                    putStrLn $ "Estudiante con ID " ++ id ++ " y nombre " ++ nombre ++ " ingresado a la universidad."
                    guardarUniversidad universidadActualizada
                    cicloPrincipal universidadActualizada
                Left error -> do
                    putStrLn error
                    cicloPrincipal universidad

        "2" -> do
            putStrLn "Ingrese el ID del estudiante a salir:"
            id <- getLine
            tiempoActual <- getCurrentTime
            case registrarSalida id tiempoActual universidad of
                Right universidadActualizada -> do
                    putStrLn $ "Estudiante con ID " ++ id ++ " ha salido de la universidad."
                    guardarUniversidad universidadActualizada
                    cicloPrincipal universidadActualizada
                Left error -> do
                    putStrLn error
                    cicloPrincipal universidad

        "3" -> do
            putStrLn "Ingrese el ID del estudiante a buscar:"
            id <- getLine
            tiempoActual <- getCurrentTime
            case buscarEstudianteActivo id universidad of
                Just estudiante -> do
                    putStrLn $ "Estudiante encontrado:"
                    putStrLn $ mostrarInfoEstudiante estudiante tiempoActual
                Nothing -> putStrLn "Estudiante no encontrado en la universidad o ya ha salido."
            cicloPrincipal universidad

        "4" -> do
            putStrLn "Ingrese el nombre del estudiante a buscar:"
            nombre <- getLine
            tiempoActual <- getCurrentTime
            let estudiantesEncontrados = buscarEstudiantePorNombre nombre universidad
            if null estudiantesEncontrados
                then putStrLn "No se encontraron estudiantes con ese nombre en la universidad."
                else do
                    putStrLn $ "Estudiantes encontrados con nombre " ++ nombre ++ ":"
                    mapM_ (\e -> putStrLn $ mostrarInfoEstudiante e tiempoActual) estudiantesEncontrados
            cicloPrincipal universidad

        "5" -> do
            listarEstudiantes universidad
            cicloPrincipal universidad

        "6" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal universidad