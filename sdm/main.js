/*
Copyright (C) 2017, University of Kansas Center for Research

Lifemapper Project, lifemapper [at] ku [dot] edu,
Biodiversity Institute,
1345 Jayhawk Boulevard, Lawrence, Kansas, 66045, USA

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.
*/

"use strict";

var app = Elm.Main.fullscreen(sdmFlags);

app.ports.fileSelected.subscribe(function(id) {
    var node = document.getElementById(id);
    var reader = new FileReader();

    var file = node.files[0];

    reader.onload = (function(event) {
        app.ports.fileContentRead.send({contents: event.target.result, filename: file.name});
    });

    reader.readAsText(file);
});



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

