"use strict";

var app = Elm.Main.fullscreen(sdmFlags);

var maps = {};
var mapLayers = {};


function configureMap(element) {
    var map = maps[element._leaflet_id];
    if (map == null) return;
    console.log("updating leaflet id", element._leaflet_id);

    var layers = mapLayers[element._leaflet_id];
    if (layers != null) {
        layers.forEach(function(layer) {  map.removeLayer(layer); });
    }

    var wmsInfos = JSON.parse(element.dataset.leaflet);

    mapLayers[element._leaflet_id] = wmsInfos.map(function(wmsInfo) {
        return L.tileLayer.wms(wmsInfo.endPoint, {
            mapName: wmsInfo.mapName,
            format: 'image/png',
            version: '1.1.0',
            transparent: true,
            layers: wmsInfo.layers.join(',')
        }).addTo(map);
    });
}


var observer = new MutationObserver(function(mutations) {
    mutations.forEach(function(m) {
        m.addedNodes.forEach(function(n) {
            if (n.getElementsByClassName == null) return;

            var elements = n.getElementsByClassName("leaflet-map");
            Array.prototype.forEach.call(elements, function(element) {
                var map = L.map(element, {crs: L.CRS.EPSG4326})
                    .setView([0, 0], 1);
                maps[element._leaflet_id] = map;
                console.log("added leaflet id", element._leaflet_id);
                configureMap(element);
            });
        });

        m.removedNodes.forEach(function(n) {
            if (n.getElementsByClassName == null) return;

            var elements = n.getElementsByClassName("leaflet-map");
            Array.prototype.forEach.call(elements, function(element) {
                if (element._leaflet_id != null) {
                    console.log("removing map with leaflet id", element._leaflet_id);
                    maps[element._leaflet_id].remove();
                    maps[element._leaflet_id] = null;
                    mapLayers[element._leaflet_id] = null;
                }
            });
        });

        if (m.type == "attributes") {
            configureMap(m.target);
        }
    });
});

observer.observe(document.body, {
    subtree: true,
    childList: true,
    attributes: true,
    attributeFilter: ["data-leaflet"],
    attributeOldValue: true
});

