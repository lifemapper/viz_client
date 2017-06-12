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

var observer = new MutationObserver(function(mutations) {
    mutations.forEach(function(m) {
        m.addedNodes.forEach(function(n) {
            if (n.getElementsByClassName == null) return;

            var elements = n.getElementsByClassName("leaflet-map");
            Array.prototype.forEach.call(elements, function(element) {
                var id = element.id;
                console.log("adding map to ", id);
                if (maps[id] == null) {
                    maps[id] = L.map(id, {crs: L.CRS.EPSG4326}).setView([0, 0], 1);
                    updateMap(id);
                }
            });
        });

        m.removedNodes.forEach(function(n) {
            if (n.getElementsByClassName == null) return;

            var elements = n.getElementsByClassName("leaflet-map");
            Array.prototype.forEach.call(elements, function(element) {
                var id = element.id;
                if (maps[id] != null) {
                    console.log("removing map from ", id);
                    maps[id].remove();
                    maps[id] = null;
                }
            });
        });
    });
});

observer.observe(document.body, { subtree: true, childList: true });


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
