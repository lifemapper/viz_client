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


var maps = {};
var mapLayers = {};

function configureMap(element) {
    var map = maps[element._leaflet_id];
    if (map == null) return;
    console.log("updating leaflet id", element._leaflet_id);

    var layers = mapLayers[element._leaflet_id];
    // if (layers != null) {
    //     layers.forEach(function(layer) {  map.removeLayer(layer); });
    // }

    var variable = element.dataset["mapSelectedVar"];

    console.log("adding layer");

    if (layers == null || layers.length === 0) {
        mapLayers[element._leaflet_id] = [
            L.geoJSON(sitesObserved, {
                style: style(variable),
                filter: function(feature) {
                    return Object.values(feature.properties).some(function(value) { return value !== 0.0; });
                }
            }).addTo(map)
        ];
    } else {
        layers[0].setStyle(style(variable));
    }
}

function style(variable) {
    const values = sitesObserved.features.map(function(feature) { return feature.properties[variable]; });
    const max = Math.max.apply(Math, values);
    const min = Math.min.apply(Math, values);
    return function(feature) {
        const value = (feature.properties[variable] - min) / (max - min);
        const style = {
            fillOpacity: value,
            stroke: false,
            fill: true,
            fillColor: "red"
        };
        return style;
    };
}


var bbox = turf.bbox(sitesObserved);

var observer = new MutationObserver(function(mutations) {
    mutations.forEach(function(m) {
        m.addedNodes.forEach(function(n) {
            if (n.getElementsByClassName == null) return;

            var elements = n.getElementsByClassName("leaflet-map");
            Array.prototype.forEach.call(elements, function(element) {
                var map = L.map(element, {worldCopyJump: true}).fitBounds([
                    [bbox[1], bbox[0]], [bbox[3], bbox[2]]
                ]);
                L.tileLayer('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
                    attribution: "© OpenStreetMap",
                    minZoom: 2,
                    maxZoom: 12
                }).addTo(map);

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
    attributeFilter: ["data-map-selected-var"],
    attributeOldValue: true
});


var app = Elm.StatsHeatMap.fullscreen();

app.ports.requestStats.subscribe(function() {
    const statRanges = sitesObserved.features.reduce(function(ranges, feature) {
        return Object.entries(feature.properties).reduce(function(ranges, statValue) {
            const name = statValue[0];
            const value = statValue[1];
            const curr = ranges[name];
            if (curr == null) {
                ranges[name] = {min: value, max: value};
            } else {
                ranges[name] = {min: Math.min(value, curr.min), max: Math.max(value, curr.max)};
            }
            return ranges;
        }, ranges);
    }, {});

    const stats = sitesObserved.features.map(function(feature) {
        return {id: feature.id, stats: Object.entries(feature.properties)};
    });

    app.ports.statsForSites.send({
        sitesObserved: stats,
        statNameLookup: Object.entries(statNameLookup),
        statRanges: Object.entries(statRanges)
    });
});

document.onmousemove = document.onmouseup = document.onmousedown = function(event) {
    const plot = document.getElementById("plot");
    if (plot == null) return;
    const rect = plot.getBoundingClientRect();
    app.ports.mouseEvent.send({
        eventType: event.type,
        x: event.clientX - rect.left,
        y: event.clientY - rect.top,
        ctrlKey: event.ctrlKey
    });
};

