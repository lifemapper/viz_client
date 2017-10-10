"use strict";

var app = Elm.StatsMain.fullscreen();

var maps = {};
var mapLayers = {};

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

function configureMap(element) {
    var map = maps[element._leaflet_id];
    if (map == null) return;
    console.log("updating leaflet id", element._leaflet_id);

    var layers = mapLayers[element._leaflet_id];
    // if (layers != null) {
    //     layers.forEach(function(layer) {  map.removeLayer(layer); });
    // }

    var sites = element.dataset["mapSites"].split(" ");

    console.log("adding layer");

    if (layers == null || layers.length === 0) {
        mapLayers[element._leaflet_id] = [
            L.geoJSON(fakeData, {style: style(sites)}).addTo(map)
        ];
    } else {
        layers[0].setStyle(style(sites));
    }
}

function style(sites) {
    return function(feature) {
        const site = "" + feature.properties.siteid;
        const style = {
            fillOpacity: 0.6,
            stroke: false,
            fill: true,
            fillColor: sites.includes(site) ? "red" : "black"
        };
        return style;
    };
}

var centers = turf.featureCollection(
    turf.featureReduce(fakeData, function(centers, feature) {
        return centers.concat(
            turf.point([feature.properties.centerX, feature.properties.centerY], feature.properties)
        );
    }, [])
);

var bbox = turf.bbox(fakeData);

var observer = new MutationObserver(function(mutations) {
    mutations.forEach(function(m) {
        m.addedNodes.forEach(function(n) {
            if (n.getElementsByClassName == null) return;

            var elements = n.getElementsByClassName("leaflet-map");
            Array.prototype.forEach.call(elements, function(element) {
                var map = L.map(element).fitBounds([
                    [bbox[1], bbox[0]], [bbox[3], bbox[2]]
                ]);
                L.tileLayer('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
                    minZoom: 1,
                    maxZoom: 12
                }).addTo(map);

                var editableLayers = new L.FeatureGroup();
                map.addLayer(editableLayers);

                var drawControl = new L.Control.Draw({
                    draw: {
                        polyline: false,
                        marker: false,
                        circle: false,
                        circlemarker: false
                    }
                });
                map.addControl(drawControl);

                map.on(L.Draw.Event.CREATED, function(e) {
                    editableLayers.addLayer(e.layer);
                    app.ports.sitesSelected.send(
                        turf.featureReduce(
                            turf.within(centers, editableLayers.toGeoJSON()),
                            function(sites, feature) {
                                return sites.concat(feature.properties.siteid);
                            }, [])
                    );
                });

                map.on(L.Draw.Event.DRAWSTART, function(e) {
                    editableLayers.clearLayers();
                });

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
    attributeFilter: ["data-map-sites"],
    attributeOldValue: true
});

