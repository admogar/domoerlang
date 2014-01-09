-module(ejemplo).
-export([ejemplo/0]).


%% Ejemplo de ejecución en línea de comandos

ejemplo() ->

% Iniciar el pool
sensor_pool:start(),

% Iniciar el master
client:start(),


% Parar el master (envío de información del nodo actual)
client:stop(node())

.