"use strict";

var app = Elm.Main.fullscreen();

var mapModels = {};
var maps = {};
var mapLayers = {};


app.ports.setLeafletMap.subscribe(function(leafletMap) {
    mapModels[leafletMap.containerId] = leafletMap;
    updateMap(leafletMap.containerId);
});

app.ports.clearLeafletMap.subscribe(function(containerId) {
    mapModels[containerId] = null;
    updateMap(containerId);
});


window.setInterval(function() {
    var mapContainers = document.getElementsByClassName("leaflet-map");
    Array.prototype.forEach.call(mapContainers, function(mapContainer) {
        var id = mapContainer.id;
        if (maps[id] == null) {
            maps[id] = L.map(id, {crs: L.CRS.EPSG4326}).setView([0, 0], 1);
            updateMap(id);
        }
    });

    Object.keys(maps).forEach(function(id) {
        if (document.getElementById(id) == null) {
            maps[id].remove();
            delete maps[id];
        }
    });
}, 100);

function updateMap(id) {
    var map = maps[id];
    if (map == null) return;

    var mapModel = mapModels[id];

    var layer = mapLayers[id];
    layer == null || map.removeLayer(layer);

    mapLayers[id] = mapModel && L.tileLayer.wms(mapModel.endPoint, {
        mapName: mapModel.mapName,
        format: 'image/png',
        version: '1.1.0',
        layers: mapModel.layers.join(',')
    }).addTo(map);
}
