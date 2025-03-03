import Data.Time.Clock 
import Data.List
import System.IO
import Control.Exception
import Control.DeepSeq (deepseq)
import System.Directory (doesFileExist)
import Data.Maybe (isNothing, mapMaybe)  -- Añade esta línea
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock (UTCTime, NominalDiffTime)

-- Definición del tipo de datos para representar la información de un estudiante
data Estudiante = Estudiante {
    idEstudiante :: String, -- Definimos el ID del estudiante como una cadena de texto
    nombreEstudiante :: String, -- Definimos el nombre del estudiante como una cadena de texto
    entrada :: UTCTime, -- Definimos la fecha de entrada como un tipo de dato de fecha y hora
    salida :: Maybe UTCTime  -- Usamos Maybe para representar que el estudiante aún está en la universidad o ya salió 
} deriving (Show, Read) -- Permite que el tipo Estudiante pueda mostrarse y leerse como texto

-- Función para registrar la entrada de un estudiante a la universidad
registrarEntrada :: String -> String -> UTCTime -> [Estudiante] -> Either String [Estudiante]
-- Toma el ID, nombre, tiempo actual y lista de estudiantes, devuelve un mensaje de error o lista actualizada de estudiantes
registrarEntrada id nombre tiempo universidad =
    case buscarEstudianteActivo id universidad of -- Buscamos si ya existe un estudiante con ese ID
        Just _ -> Left $ "Error: Ya existe un estudiante con ID " ++ id ++ " dentro de la universidad." -- Si existe, retornamos error
        Nothing -> Right $ Estudiante id nombre tiempo Nothing : universidad -- Si no existe, añadimos nuevo estudiante a la lista

-- Función para registrar la salida de un estudiante de la universidad
registrarSalida :: String -> UTCTime -> [Estudiante] -> Either String [Estudiante]
registrarSalida id tiempo universidad =
    case buscarEstudianteActivo id universidad of -- Buscamos al estudiante activo con ese ID
        Just _ -> Right $ map (\e -> if id == idEstudiante e && isNothing (salida e) -- Si es el estudiante buscado, registramos su salida
                                    then e { salida = Just tiempo } 
                                    else e) universidad -- Si no es el estudiante buscado, lo dejamos igual
        Nothing -> Left $ "Error: No se encontró un estudiante activo con ID " ++ id ++ " en la universidad." -- Si no existe, retornamos error

-- Función para buscar un estudiante activo por su ID en la universidad
buscarEstudianteActivo :: String -> [Estudiante] -> Maybe Estudiante -- Busca un estudiante que coincida con el ID y no tenga registro de salida
buscarEstudianteActivo id universidad = -- Buscamos al estudiante activo con ese ID 
    find (\e -> id == idEstudiante e && isNothing (salida e)) universidad

-- Función para buscar un estudiante por su nombre en la universidad
buscarEstudiantePorNombre :: String -> [Estudiante] -> [Estudiante]
-- filtramos la lista de estudiantes por nombre que coincidan con el dado y que no hayan registrado salida. 
buscarEstudiantePorNombre nombre universidad = 
    filter (\e -> nombre == nombreEstudiante e && isNothing (salida e)) universidad -- Buscamos estudiante con ID coincidente y sin salida

-- Función para calcular el tiempo que un estudiante ha permanecido en la universidad
tiempoEnUniversidad :: Estudiante -> UTCTime -> NominalDiffTime
tiempoEnUniversidad estudiante tiempoActual = -- Calculamos el tiempo que ha pasado desde la entrada o salida del estudiante
    case salida estudiante of -- Verificamos si el estudiante ya salió
        Just tiempoSalida -> diffUTCTime tiempoSalida (entrada estudiante) -- Si salió, calculamos el tiempo desde la entrada hasta la salida
        Nothing -> diffUTCTime tiempoActual (entrada estudiante) -- Si no salió, calculamos el tiempo desde la entrada hasta el momento actual

-- Función para formatear el tiempo en horas, minutos y segundos
formatearTiempo :: NominalDiffTime -> String
formatearTiempo tiempo = -- Convierte segundos a formato legible de horas, minutos y segundos
    let totalSegundos = floor tiempo :: Integer  -- Convertimos a segundos totales
        horas = totalSegundos `div` 3600 -- Calculamos horas
        minutos = (totalSegundos `mod` 3600) `div` 60 -- Calculamos minutos
        segundos = totalSegundos `mod` 60 -- Calculamos segundos
    in show horas ++ " horas, " ++ show minutos ++ " minutos, " ++ show segundos ++ " segundos" -- Formateamos el resultado

-- Función auxiliar para formatear la fecha y hora sin decimales
formatearFechaHora :: UTCTime -> String -- Formatea la fecha y hora en un formato específico
-- Formatea la fecha y hora en formato YYYY-MM-DD, HH:MM:SS
formatearFechaHora = formatTime defaultTimeLocale "%Y-%m-%d, %H:%M:%S"  -- Elimina fracciones de segundo

-- Función para guardar la información de los estudiantes en un archivo de texto
guardarUniversidad :: [Estudiante] -> IO ()
-- Escribe la lista de estudiantes en un archivo de texto
guardarUniversidad universidad = do
    withFile "University.txt" WriteMode $ \h -> do -- Abrimos el archivo en modo escritura
        hPutStr h (unlines (map mostrarEstudiante universidad))  -- Escribimos cada estudiante en una línea
    putStrLn "Cambios guardados en el archivo University.txt." -- Mostramos mensaje de confirmación

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
                estudiantes = mapMaybe leerEstudianteSafe lineas
            return estudiantes
        else do
            putStrLn "El archivo University.txt no existe. Se creará uno nuevo."
            return []
  where
    leerEstudianteSafe :: String -> Maybe Estudiante
    leerEstudianteSafe linea =
      case reads linea :: [(Estudiante, String)] of
        [(e, "")] -> Just e
        _         -> Nothing
-- Función para mostrar la información de un estudiante como cadena de texto
mostrarEstudiante :: Estudiante -> String 
-- Muestra la información de un estudiante en formato de cadena de texto
mostrarEstudiante (Estudiante id nombre entrada salida) = -- Muestra la información de un estudiante en un formato compatible con read
    "Estudiante {idEstudiante = \"" ++ id ++ "\", nombreEstudiante = \"" ++ nombre ++ "\", entrada = " ++ show entrada ++ ", salida = " ++ maybe "Nothing" show salida ++ "}"

-- Función para listar los estudiantes en la universidad
listarEstudiantes :: [Estudiante] -> IO ()
-- Muestra la lista de estudiantes en la universidad
listarEstudiantes [] = putStrLn "No hay estudiantes en la universidad." -- Si no hay estudiantes
listarEstudiantes estudiantes = do -- Si hay estudiantes
    tiempoActual <- getCurrentTime -- Obtenemos tiempo actual
    putStrLn "Estudiantes en la universidad:"
    mapM_ (\e -> putStrLn $ mostrarInfoEstudiante e tiempoActual) estudiantes -- Mostramos cada estudiante

-- Función para mostrar información detallada de un estudiante
mostrarInfoEstudiante :: Estudiante -> UTCTime -> String
-- Formatea la información de un estudiante para mostrar en pantalla
mostrarInfoEstudiante estudiante tiempoActual =
    "ID: " ++ idEstudiante estudiante ++ 
    ", Nombre: " ++ nombreEstudiante estudiante ++ 
    ", Entrada: " ++ formatearFechaHora  (entrada estudiante) ++ 
    ", Salida: " ++ maybe "Aún en la universidad" formatearFechaHora (salida estudiante) ++
    ", Tiempo: " ++ formatearTiempo (tiempoEnUniversidad estudiante tiempoActual)

-- Función principal del programa
main :: IO ()
main = do
    -- Cargar la universidad desde el archivo de texto
    universidad <- cargarUniversidad
    putStrLn "¡Bienvenido al Sistema de Gestión de Estudiantes Universitarios!"

    cicloPrincipal universidad -- Iniciar el ciclo principal del programa

-- Menú principal del programa
cicloPrincipal :: [Estudiante] -> IO ()
cicloPrincipal universidad = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de estudiante"
    putStrLn "2. Registrar salida de estudiante"
    putStrLn "3. Buscar estudiante por ID"
    putStrLn "4. Buscar estudiante por nombre"
    putStrLn "5. Listar estudiantes"
    putStrLn "6. Salir"

    opcion <- getLine -- Leer la opción seleccionada
    case opcion of -- Realizar acción según la opción seleccionada
        "1" -> do -- Registrar entrada de estudiante
            putStrLn "Ingrese el ID del estudiante:"
            id <- getLine -- Leemos el ID del estudiante
            putStrLn "Ingrese el nombre del estudiante:"
            nombre <- getLine -- Leemos el nombre del estudiante
            tiempoActual <- getCurrentTime -- Obtenemos tiempo actual
            case registrarEntrada id nombre tiempoActual universidad of -- Hacemos el intento de registar la entrada del estudiante
                Right universidadActualizada -> do -- Si se registró correctamente
                    putStrLn $ "Estudiante con ID " ++ id ++ " y nombre " ++ nombre ++ " ingresado a la universidad."
                    guardarUniversidad universidadActualizada -- Guardamos la universidad actualizada en el archivo
                    esperar -- Función que evita el acumulamiento de información en la terminal 
                    cicloPrincipal universidadActualizada -- Volvemos al menú principal
                Left error -> do -- Si hubo un error al registrar la entrada
                    putStrLn error -- Mostramos el error
                    esperar -- Función que evita el acumulamiento de información en la terminal
                    cicloPrincipal universidad -- Volvemos al menú principal

        "2" -> do -- Registrar salida de estudiante
            putStrLn "Ingrese el ID del estudiante a salir:" -- Pedimos el ID del estudiante a salir
            id <- getLine -- Leemos el ID del estudiante a salir
            tiempoActual <- getCurrentTime -- Obtenemos tiempo actual
            case registrarSalida id tiempoActual universidad of -- Hacemos el intento de registrar la salida del estudiante
                Right universidadActualizada -> do -- Si se registró correctamente la salida del estudiante
                    putStrLn $ "Estudiante con ID " ++ id ++ " ha salido de la universidad."
                    guardarUniversidad universidadActualizada -- Guardamos la universidad actualizada en el archivo
                    esperar -- Función que evita el acumulamiento de información en la terminal
                    cicloPrincipal universidadActualizada -- Volvemos al menú principal
                Left error -> do -- Si hubo un error al registrar la salida del estudiante
                    putStrLn error -- Mostramos el error
                    esperar -- Función que evita el acumulamiento de información en la terminal
                    cicloPrincipal universidad -- Volvemos al menú principal

        "3" -> do   -- Buscar estudiante por ID
            putStrLn "Ingrese el ID del estudiante a buscar:" -- Pedimos el ID del estudiante a buscar
            id <- getLine -- Leemos el ID del estudiante a buscar
            tiempoActual <- getCurrentTime -- Obtenemos tiempo actual
            case buscarEstudianteActivo id universidad of -- Buscamos el estudiante activo con ese ID
                Just estudiante -> do -- Si encontramos el estudiante activo
                    putStrLn $ "Estudiante encontrado:"
                    putStrLn $ mostrarInfoEstudiante estudiante tiempoActual -- Mostramos la información del estudiante
                Nothing -> putStrLn "Estudiante no encontrado en la universidad o ya ha salido." -- Si no encontramos el estudiante activo mandamos mensaje de error
            esperar -- Función que evita el acumulamiento de información en la terminal
            cicloPrincipal universidad -- Volvemos al menú principal

        "4" -> do -- Buscar estudiantes por nombre
            putStrLn "Ingrese el nombre del estudiante a buscar:" -- Pedimos el nombre del estudiante a buscar
            nombre <- getLine -- Leemos el nombre del estudiante a buscar
            tiempoActual <- getCurrentTime -- Obtenemos tiempo actual
            let estudiantesEncontrados = buscarEstudiantePorNombre nombre universidad -- Buscamos estudiantes con ese nombre
            if null estudiantesEncontrados -- Condicional si no encontramos ningun estudiante con ese nombre
                then putStrLn "No se encontraron estudiantes con ese nombre en la universidad." -- Mensaje de error
                else do -- Si encontramos al menos un estudiante con ese nombre
                    putStrLn $ "Estudiantes encontrados con nombre " ++ nombre ++ ":" -- Mostramos el mensaje de que encontramos estudiantes con ese nombre
                    mapM_ (\e -> putStrLn $ mostrarInfoEstudiante e tiempoActual) estudiantesEncontrados -- Mostramos la información de los estudiantes encontrados
            esperar -- Función que evita el acumulamiento de información en la terminal
            cicloPrincipal universidad  -- Volvemos al menú principal

        "5" -> do -- Mostrar estudiantes de la universidad en una lista
            listarEstudiantes universidad -- Listar estudiantes en la universidad
            esperar -- Función que evita el acumulamiento de información en la terminal
            cicloPrincipal universidad -- Volvemos al menú principal

        "6" -> putStrLn "¡Hasta luego, vuelva pronto!" -- Mensaje de despedida

        _ -> do -- Si el usuario ingresa una opción no válida
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal universidad -- Volvemos al menú principal

esperar :: IO () -- Función para esperar a que el usuario presione Enter
esperar = do -- Espera a que el usuario presione Enter
    putStrLn "\nPresiona Enter para continuar..." -- Pedimos al usuario que presione Enter
    _ <- getLine -- Esperamos a que el usuario presione Enter
    return () -- Retornamos un valor vacío, es decir, no hacemos nada con la entrada del usuario