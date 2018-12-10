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

var files = {};

app.ports.fileSelected.subscribe(function(args) {
    var id = args[0], doPreview = args[1];
    var node = document.getElementById(id);
    var file = node.files[0];
    files[id] = file;
    if (doPreview) {
        Papa.parse(file, {
            preview: 4,
            complete: function(results) {
                app.ports.selectedFileName.send({id: id, filename: file.name, preview: results.data});
            }
        });
    } else {
        app.ports.selectedFileName.send({id: id, filename: file.name, preview: []});
    }
});

app.ports.uploadCmd.subscribe(function(info) {
    var xhr = new XMLHttpRequest();
    xhr.upload.addEventListener("progress", function(evt) {
        evt.lengthComputable && app.ports.uploadProgress.send({
            id: info.id,
            loaded: evt.loaded,
            total: evt.total
        });
    });
    xhr.addEventListener("load", function(evt) {
        console.log("load", evt, xhr);
        app.ports.uploadComplete.send({id: info.id, response: xhr.responseText, status: xhr.status});
    });
    xhr.addEventListener("error", function(evt) {
        console.log("error", evt, xhr);
        app.ports.uploadFailed.send({id: info.id, response: xhr.responseText});
    });
    xhr.addEventListener("abort", function(evt) {
        console.log("abort", evt, xhr);
        app.ports.uploadCanceled.send(info.id);
    });
    xhr.open("POST", info.url, true);
    xhr.responseType = "text";
    xhr.setRequestHeader("Content-type", "application/octet-stream");
    xhr.send(files[info.id]);
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
    var bb = element.dataset.leafletBoundingBox && JSON.parse(element.dataset.leafletBoundingBox);

    mapLayers[element._leaflet_id] = wmsInfos.map(function(wmsInfo) {
        bb && map.fitBounds(bb);

        return L.tileLayer.wms(wmsInfo.endPoint, {
            mapName: wmsInfo.mapName,
            format: 'image/png',
            version: '1.1.0',
            transparent: true,
            crs: L.CRS.EPSG4326,
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
                var map = L.map(element, {minZoom: 2})
                    .setView([0, 0], 1);
                var tandemMoveHandler = tandemMove(map);
                map.on('moveend', tandemMoveHandler).on('zoomend', tandemMoveHandler);
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
                    delete maps[element._leaflet_id];
                    delete mapLayers[element._leaflet_id];
                }
            });
        });

        if (m.type == "attributes") {
            configureMap(m.target);
        }
    });
});

var moving = false;
function tandemMove(map) {
    return function(event) {
        if (moving) return;
        moving = true;
        for (var leafletId in maps) {
            if (map !== maps[leafletId]) {
                maps[leafletId].setView(map.getCenter(), map.getZoom());
            }
        }
        moving = false;
    };
}

observer.observe(document.body, {
    subtree: true,
    childList: true,
    attributes: true,
    attributeFilter: ["data-leaflet", "data-leaflet-bounding-box"],
    attributeOldValue: true
});

