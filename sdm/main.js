"use strict";

var app = Elm.Main.fullscreen(sdmFlags);

var maps = {};
var mapLayers = {};


app.ports.setLeafletMap.subscribe(function(leafletMap) {
    var map = maps[leafletMap.id];
    if (map == null) return;

    var layer = mapLayers[leafletMap.id];
    layer == null || map.removeLayer(layer);

    var wmsInfo = leafletMap.state.wmsLayer;

    mapLayers[leafletMap.id] = wmsInfo && L.tileLayer.wms(wmsInfo.endPoint, {
        mapName: wmsInfo.mapName,
        format: 'image/png',
        version: '1.1.0',
        transparent: true,
        layers: wmsInfo.layers.join(',')
    }).addTo(map);

    var view = leafletMap.state.view;

    map.setView({lat: view[0], lng: view[1]}, view[2], {animate: false});
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
                    maps[id] = L.map(id, {crs: L.CRS.EPSG4326})
                        .setView([0, 0], 1)
                        .on('moveend', function(event) {
                            var center = maps[id].getCenter();
                            var zoom = maps[id].getZoom();

                            app.ports.leafletViewChanged.send([id, [center.lat, center.lng, zoom]]);
                        });
                    app.ports.leafletRequestState.send(id);
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
                    mapLayers[id] = null;
                }
            });
        });
    });
});

observer.observe(document.body, { subtree: true, childList: true });

