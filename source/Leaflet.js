"use strict";

var app = Elm.Main.fullscreen();

var maps = {};

app.ports.drawMap.subscribe(function([id, view]) {
    window.setTimeout(function() {
        var map = L.map(id, {crs: L.CRS.EPSG4326}).setView([view.lat, view.lon], view.zoom);
        maps[id] = map;

        var wmsOptions = {
            mapName: 'scen_AR5-CCSM4-RCP8.5-2070-10min',
            format: 'image/png',
            version: '1.1.0',
            layers: 'bio1-AR5-CCSM4-RCP8.5-2070-10min'
        };

        var wmsLayer = L.tileLayer.wms('http://notyeti-191.lifemapper.org/api/v2/ogc?', wmsOptions).addTo(map);
    }, 1000);
});

app.ports.destroyMap.subscribe(function(id) {
    maps[id].remove();
});

app.ports.setMap.subscribe(function([id, view]) {
    maps[id].setView([view.lat, view.lon], view.zoom);
});
