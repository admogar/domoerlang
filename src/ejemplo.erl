-module(ejemplo).
-export([ejemplo/0]).


%% Ejemplo de ejecución en línea de comandos

ejemplo() ->

% Iniciar el pool
sensor_pool:start(),

% Iniciar el master
client:start(),

master:anadir_sensor("cocina", "luz"),
master:obtener_grupos(),

% Obtener estado. Devuelve una lista tuplas con la siguiente información:
% { nombre sensor,
%   lectura del sensor en la caché,
%   tiempo transcurrido desde el último heartbeat}
master:obtener_estado_grupo("cocina"),

% Parar el master (envío de información del nodo actual)
client:stop(node())

.   