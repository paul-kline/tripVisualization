
var map, heatmap, heatmapdat, globaljsontrips;

function toggleHeatmap() {
    document.getElementById("heatmapbutton").innerHTML = "Loading..."
    let confirmed = false;
    if (heatmap == undefined || heatmap == null) {
        //asking for first time.
        let b = confirm("Warning: Viewing data distribution can significantly hinder interactivity! View anyway?");
        if (!b) {
            return;
        } else {
            confirmed = true;
        }
        createHeatmap();
    }

    if (!heatmap.getMap()) {
        //if the heatmap is not currently shown:
        //and I didn't ask already:
        if (!confirmed) {
            b = confirm("Warning: Viewing data distribution can significantly hinder interactivity! View anyway?");
            if (b) {
                heatmap.setMap(heatmap.getMap() ? null : map);
                
            }
        } else {
            //confirmed is true
            heatmap.setMap(heatmap.getMap() ? null : map);
            
        }

    } else {
        //they are unviewing it, so let's just set to null.
        heatmap.setMap(heatmap.getMap() ? null : map);
    }

    document.getElementById("heatmapbutton").innerHTML = "Toggle Data Distribution"

}
function toggleDiss() {
    if (heatmap == undefined || heatmap == null) {
        createHeatmap();
    }
    heatmap.dissipating = !heatmap.dissipating
}
function toggleTripLines() {

    allTrips.forEach(t => {
        t.setMap(t.getMap() ? null : map);
    });

}
var MAXSPEED;
function getMaxSpeed(){
    return new Promise((resolve,reject)=>{
      $.get("./maxspeed", function (d) {
        console.log("I got: ", d);
        resolve(d.speed);
          MAXSPEED = d.speed;
        });
    });
}
function test() {
    //createHeatmap();
    let minSpeed = 0;
    console.log("in test, here is heatmapdat: ", heatmapdat)
    let maxSpeed = heatmapdat.reduce((old, n) => {
        if (old.weight > n.weight) {
            return old;
        } else {
            return n;
        }
    }).weight;
    let gradLen = gradient.length;
    heatmapdat.forEach(d => {
        let colIndex = Math.round((d.weight / maxSpeed) * gradLen);
        var marker = new google.maps.Marker({
            draggable: true,
            position: { lat: d.location.lat(), lng: d.location.lng() },
            map: map,
            title: "AveSpeed: " + d.weight //mkString(d,false)
        });
        // let circ = new google.maps.Circle({
        //     strokeColor: gradient[colIndex],
        //     strokeOpacity: 0.8,
        //     strokeWeight: 2,
        //     fillColor: gradient[colIndex],
        //     fillOpacity: 0.35,
        //     map: map,
        //     center: {lat : d.location.lat(), lng : d.location.lng()},
        //     radius: 5,
        //     draggable : false
        //   });

    });

}

var heatPointOfDataPointsDat = [];


function createHeatmap() {
    globaljsontrips.forEach(trip => {
        trip.coords.forEach(c => {
            heatPointOfDataPointsDat.push({
                location: new google.maps.LatLng(c.lat, c.lng)
            })
        });
    });
    console.log("here is heatPointOfDataPointsDat", heatPointOfDataPointsDat)
    heatmap = new google.maps.visualization.HeatmapLayer({
        data: heatPointOfDataPointsDat
    });
    return;

    //----- unreachable 
    heatmapdat = [];
    $.get("./consol", function (d) {
        console.log("I got: ", d);
        d.forEach(tripPair => {
            let lat = tripPair[0].lat
            let lng = tripPair[0].lng
            let weight = tripPair[1].maxSpeed
            heatmapdat.push({
                location: new google.maps.LatLng(lat, lng),
                weight: weight
            });

        });
        heatmap = new google.maps.visualization.HeatmapLayer({
            data: heatmapdat
            , dissipating: true
        });
    }, 'json');

}


function initMap() {


    console.log("initializing map...");
    let txtFile = './trips';
    let myPromise = new Promise((resolve, reject) => {
        $.get(txtFile, function (trips) {
            console.log("recevied trips...")
            resolve(trips);
        }, 'json');

    });
    myPromise.then(handleTrips)
    // .then(handleTrips);

}

function readInTrips(dataFiles) {

    $.getJSON(file, function (result) {
        //console.log("here is the file:" + file);//, "resolving with:", result);
        console.log(result);
        //trips.push(result);
        resolverr(result);
    });

    return Promise.all(promises);

}

function tripsToCoords(trips) {
    let res = {};
    res.tripNumber = trips.length;

    res.coords = [];

    trips.forEach((trip, index) => {
        trip.coords.forEach(coord => {
            let obj = {};
            obj.lat = coord.lat;
            obj.lng = coord.lng;

            obj.dist = coord.dist;
            obj.index = coord.index;
            obj.speed = coord.speed;
            obj.tripNumber = index;
            res.coords.push(obj);
        });

    });
    return res;



}
function mkString(d, html) {
    if (html) {
        return "Average Speed: " + d.aveSpeed.toFixed(2) +
            "<br>Min Speed: " + d.minSpeed.toFixed(2) +
            "<br>Max Speed: " + d.maxSpeed.toFixed(2) +
            "<br>based on: " + d.trips + " trips." +
            "<br>" + d.dataPoints + " data points."
    } else {
        return "Average Speed: " + d.aveSpeed.toFixed(2) + "\nMin Speed: " + d.minSpeed.toFixed(2) + "\nMax Speed: " + d.maxSpeed.toFixed(2) + "\nbased on: " + d.trips + " trips." + "\n" + d.dataPoints + " data points."
    }

}
let noDatStr = "No data present within radius";
function mkInfoWindowWithStr(str) {
    let infowindow = new google.maps.InfoWindow({
        content: str
    });
    return infowindow;
}

function mkInfoWindowWithResponse(d) {
    let str = "";
    let infowindow;
    if (undefined == d || null == d) {
        infowindow = new google.maps.InfoWindow({
            content: "No data"
        });
    } else {
        str = mkString(d, true)
        infowindow = new google.maps.InfoWindow({
            content: str
        });
    }
    return infowindow;
}
getMaxSpeed();

function mkCircle(pos, map, size, marker) {

    let circ = new google.maps.Circle({
        strokeColor: '#27AE60',
        strokeOpacity: 0.8,
        strokeWeight: 2,
        fillColor: '#27AE60',
        fillOpacity: 0.40,
        map: map,
        center: pos,
        radius: size,
        editable: true,
        draggable: false
    });
    circ.bindTo('center', marker, 'position');
    // circ.addListener('center_changed', function (e){
    //     console.log("circle dragged!!");
    //     circ.trig(e);
    // } );
    marker.circle = circ;

    
    circ.trig = function (ev) {
        console.log("radius change:", ev);
        marker.infowindow.setMap(null);
        let p = queryLocation(marker.circle.center.lat(), marker.circle.center.lng(), marker.circle.radius);
        p.then(d => {
            console.log("radiusI got: ", d);
            //marker.position = {lat : d.queriedLocation.lat, lng : d.queriedLocation.lng };
            marker.infowindow = null;
            marker.infowindow = mkInfoWindowWithResponse(d);
            let col = getColor(0,d.aveSpeed, MAXSPEED);
            marker.circle.setOptions({
                fillColor: col,
                strokeColor: col
            });
            marker.infowindow.open(map, marker)
        }).catch(badD => {
            marker.infowindow = null;
            marker.circle.setOptions({
                fillColor: '#FFFFFF',
                strokeColor: '#FFFFFF'
            });
            marker.infowindow = mkInfoWindowWithStr(noDatStr);
            marker.infowindow.open(map, marker)
        });

    };
    circ.addListener('radius_changed', circ.trig);
    return circ;
}
var allTrips = [];
function querySegments(mn, mx) {
    console.log("about toquery here is type of fist arg")
    console.log(typeof mn)
    console.log(mn)
    if(mn == ""){
        mn = "-1";
    }
    if(mx == ""){
        mx = "-1";
    }
    return new Promise((resolve, reject) => {
        $.get("./filteredTrips?minSpeed=" + mn + "&maxSpeed=" + mx, (d) => {
            console.log("filtered trips be here!  I got: ", d);
            if (typeof d == 'object') {
                console.log("resolving!")
                resolve(d)
            } else {
                console.log("rejecting!!")
                reject(d);
            }
        });
    });



}
function queryLocation(lat, lng, within) {
    let my = new Promise((resolve, reject) => {
        console.log("querying location!!")
        $.get("./query?lat=" + lat + "&lng=" + lng + "&within=" + within, (d) => {
            console.log("herewas I got: ", d);
            if (typeof d == 'object') {
                console.log("resolving!")
                resolve(d)
            } else {
                console.log("rejecting!!")
                reject(d);
            }
        });
    });
    return my;
}
function mkPolyLine(coords, color, strokeWeight) {
    if(color == null){
        color = '#000000';
    }
    if(strokeWeight == null){
        strokeWeight = 2
    }
    return new google.maps.Polyline({
        path: coords,
        geodesic: true,
        strokeColor: color,
        strokeOpacity: 1.0,
        strokeWeight: strokeWeight
    });

}

function pathlistener(dat){
        console.log(dat)
        console.log(dat.latLng.lat());
        console.log(dat.latLng.lng());
        var size = 20
        let p = queryLocation(dat.latLng.lat(), dat.latLng.lng(), size);
        let handler = (wellBool, d) => {
            var infowindow = wellBool ? mkInfoWindowWithResponse(d) : mkInfoWindowWithStr(noDatStr);

            var marker = new google.maps.Marker({
                draggable: true,
                position: { lat: dat.latLng.lat(), lng: dat.latLng.lng() }, //{ lat: d.queriedLocation.lat, lng: d.queriedLocation.lng },//
                map: map
                //,title: wellBool ? mkString(d, false) : noDatStr
            });
            infowindow.open(map, marker);

            let circ = mkCircle(dat.latLng, map, size, marker);
            let col = getColor(0,d.aveSpeed, MAXSPEED);
            circ.setOptions({
                fillColor: col,
                strokeColor: col
            });
            marker.infowindow = infowindow;
            marker.addListener('mousedown', function (dart) {
                console.log("Mouse down data: ", dart);
                console.log(dart.Aa.which)
                if (dart.Aa.which == 3) {
                    marker.circle.setMap(null)
                    marker.infowindow.setMap(null)
                    marker.setMap(null);
                    marker = null;
                }
            });

            marker.recalc = function (dat) {
                //-----------------------------------------------------
                marker.circle.setMap(null)
                marker.infowindow.setMap(null);

                mkCircle(marker.position, map, marker.circle.radius, marker);
                //clear the infoWindow;
                let p2 = queryLocation(dat.latLng.lat(), dat.latLng.lng(), marker.circle.radius);
                let f2 = (wellBool, d) => {
                    console.log("mouseup event: I got: ", d);
                    infowindow = wellBool ? mkInfoWindowWithResponse(d) : mkInfoWindowWithStr(noDatStr);
                    infowindow.open(map, marker)
                    marker.infowindow = infowindow
                    let col = getColor(0,d.aveSpeed, MAXSPEED);
                    marker.circle.setOptions({
                        fillColor: col,
                        strokeColor: col
                    });
                }
                p2.then(d => {
                    return f2(true, d);
                }).catch(d => {
                    return f2(false, d);
                })
            }
            marker.addListener('poition_changed', marker.recalc);
            marker.addListener('mouseup', marker.recalc );
            


        }
        p.then(d => {
            return handler(true, d);
        }).catch(d => {
            return handler(false, d);
        })
}
function handleTrips(trips) {
    console.log("here are my trips:", trips);

    let obj = trips[0].coords[0];
    let lat = obj.lat;
    let lng = obj.lng;
    map = new google.maps.Map(document.getElementById('map'), {
        zoom: 10,
        center: new google.maps.LatLng(lat, lng),
        mapTypeId: 'roadmap'
    });
    globaljsontrips = trips;
    trips.forEach(trip => {

        let flightPath = mkPolyLine(trip.coords);

        flightPath.addListener('click',pathlistener);
        flightPath.setMap(map);
        allTrips.push(flightPath);
    });
}


function consolidate_within_feet(data, miles) {
    let averaged = [];
    data.forEach(coordDat => {
        console.log("adding coord:", coordDat.tripNumber);
        addDataToAveraged(averaged, coordDat, miles);

    });
    return averaged;
}
function addDataToAveraged(average, newEntry, miles) {
    let obj = average.find(x => distance(x.lat, x.lng, newEntry.lat, newEntry.lng) < miles);
    if (obj == undefined) {
        obj = {};
        obj.dataPoints = 1; //this first one.
        obj.lat = newEntry.lat;
        obj.lng = newEntry.lng;
        obj.aveSpeed = newEntry.speed;
        average.push(obj);
        console.log(average.length)
    } else {
        console.log("FOUND")
        obj.dataPoints += 1;
        obj.aveSpeed = obj.aveSpeed * ((obj.dataPoints - 1) / obj.dataPoints) + newEntry.speed * (1 / obj.dataPoints);


    }



}
function distance(lat1, lon1, lat2, lon2, unit) {
    var radlat1 = Math.PI * lat1 / 180
    var radlat2 = Math.PI * lat2 / 180
    var theta = lon1 - lon2
    var radtheta = Math.PI * theta / 180
    var dist = Math.sin(radlat1) * Math.sin(radlat2) + Math.cos(radlat1) * Math.cos(radlat2) * Math.cos(radtheta);
    dist = Math.acos(dist)
    dist = dist * 180 / Math.PI
    dist = dist * 60 * 1.1515
    if (unit == "K") { dist = dist * 1.609344 }
    if (unit == "N") { dist = dist * 0.8684 }
    return dist
}
var allPolySegs = [];
function filterSegments() {
    document.getElementById("filterbutton").innerHTML = "loading..."

    allPolySegs.forEach(seg => {
        seg.setMap(null);

    });
    allPolySegs = [];

    let xx = document.getElementById("maxSpeed").value //$("#maxSpeed").val()
    let mm = document.getElementById("minSpeed").value //$("#minSpeed").val()
    console.log("in filtersegements")
    console.log(mm, xx);
    console.log(typeof mm, typeof xx)
    // console.log("calling to string on them")
    let myP = querySegments(mm, xx);
    //if all trips are currently visible, hide them!! hide them now!!
    // if (allTrips[0].getMap() != null) {
    //     toggleTripLines();
    // }

    myP.then(allSegs => {
        allSegments = allSegs;
        allSegments.forEach(seg => {
            setTimeout(() => {
                let pol = mkPolyLine(seg.coords,'#FF0000', 4);
                pol.addListener('click',pathlistener);
                pol.setMap(map);
                allPolySegs.push(pol);
            }, 0);
        });
        document.getElementById("filterbutton").innerHTML = "Filter Segements"
        

    }).catch(e=>{
        console.log(e);
        alert("invalid filter applied")
        document.getElementById("filterbutton").innerHTML = "Filter Segements"
    })

}

//https://www.strangeplanet.fr/work/gradient-generator/index.php
let gradient = ["#47FF56", "#4AF954", "#4EF452", "#52EF50", "#56EA4E", "#59E44D", "#5DDF4B", "#61DA49", "#65D547", "#68D046", "#6CCA44", "#70C542", "#74C040", "#77BB3F", "#7BB63D", "#7FB03B", "#83AB39", "#86A638", "#8AA136", "#8E9C34", "#929632", "#959131", "#998C2F", "#9D872D", "#A1822B", "#A47C2A", "#A87728", "#AC7226", "#B06D24", "#B36823", "#B76221", "#BB5D1F", "#BF581D", "#C2531C", "#C64E1A", "#CA4818", "#CE4316", "#D13E15", "#D53913", "#D93411", "#DD2E0F", "#E0290E", "#E4240C", "#E81F0A", "#EC1A08", "#EF1407", "#F30F05", "#F70A03", "#FB0501", "#FF0000"]
function getColor(min,cur,max){
    if(cur <= min){
        return gradient[0];
    }
    if(cur >= max){
        return gradient[gradient.length-1];
    }
    let diff = max - min;
    let x = (cur - min)/diff * gradient.length
    return gradient[Math.round(x)]

}

