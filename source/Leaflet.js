"use strict";

var app = Elm.Main.fullscreen();
var map = null;
var mapState = null;
var wmsLayer = null;

var id = "leaflet-map";

window.setInterval(function() {
    var div = document.getElementById(id);
    if (div == null && map != null) {
        map.remove();
        map = null;
    } else if (div != null && map == null && mapState != null) {
        addMap();
    }
}, 100);

function updateMap() {
    if (map != null) {
        if (wmsLayer != null) map.removeLayer(wmsLayer);

        wmsLayer = mapState == null ? null : L.tileLayer.wms(mapState.endPoint, {
            mapName: mapState.mapName,
            format: 'image/png',
            version: '1.1.0',
            layers: mapState.layers.join(',')
        }).addTo(map);
    }
}

app.ports.updateMapState.subscribe(function(state) {
    mapState = (state.mapName == "" || state.endPoint == "") ? null : state;
    updateMap();
});

function addMap() {
    map = L.map(id, {crs: L.CRS.EPSG4326}).setView([0, 0], 1);
    updateMap();
}
