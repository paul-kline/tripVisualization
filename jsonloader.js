function initMap(){



    let txtFile = './tripNames.txt';
    let myPromise = new Promise((resolve,reject)=> {
        $.get(txtFile, function(data) {
            resolve( data.split("\n"));
            }, 'text');
    
    });
    myPromise.then(readInTrips)
    .then(handleTrips);

}

function readInTrips(dataFiles){
    //let trips = [];

    let promises = dataFiles.filter(fname => fname.length > 1).map(file => new Promise((resolverr,reject)=>{
            $.getJSON(file, function(result){
                //console.log("here is the file:" + file);//, "resolving with:", result);
                //console.log(result);
                //trips.push(result);
                resolverr(result);
            });
        })
    );
    console.log("promises: " , promises);
    //let res = await Promise.all(promises);
    return Promise.all(promises);

}

function tripsToCoords(trips){
    let res = {};
    res.tripNumber = trips.length;

    res.coords = [];
    
    trips.forEach((trip,index)=>{
        trip.coords.forEach(coord=>{
            let obj = {};
            obj.lat = coord.lat;
            obj.lng = coord.lng;
            
            obj.dist =coord.dist;
            obj.index = coord.index;  
            obj.speed = coord.speed;
            obj.tripNumber = index;
            res.coords.push(obj);
        });
        
    });
    return res;



}

let map;
function handleTrips(trips){
    console.log(trips);

    // var flightPlanCoordinates = [
    //     {lat: 37.772, lng: -122.214},
    //     {lat: 21.291, lng: -157.821},
    //     {lat: -18.142, lng: 178.431},
    //     {lat: -27.467, lng: 153.027}
    //   ];
    //   var flightPath = new google.maps.Polyline({
    //     path: flightPlanCoordinates,
    //     geodesic: true,
    //     strokeColor: '#FF0000',
    //     strokeOpacity: 1.0,
    //     strokeWeight: 2
    //   });
    
    //   flightPath.setMap(map);
    // }
    let coordObj = tripsToCoords(trips);
    alert(coordObj.coords.length)
    let averaged = consolidate_within_feet(coordObj.coords,2);
    console.log(averaged)
    let obj = trips[0].coords[0];
    let lat = obj.lat;
    let lng = obj.lng;
    map = new google.maps.Map(document.getElementById('map'), {
          zoom: 10,
          center: new google.maps.LatLng(lat,lng),
          mapTypeId: 'roadmap'
        });
    let heatMapData = [];
    averaged.forEach(c =>{
        let obj = {};
                obj.location = new google.maps.LatLng(c.lat,c.lng);
                obj.weight = c.aveSpeed
                heatMapData.push(obj);

    });
    // trips.forEach(trip=>{
    //     trip.coords.forEach(c =>{
    //         let obj = {};
    //         obj.location = new google.maps.LatLng(c.lat,c.lng);
    //         obj.weight = c.speed
    //         heatMapData.push(obj);
    //     });
    //     let flightPath = new google.maps.Polyline({
    //         path: trip.coords,
    //         geodesic: true,
    //         strokeColor: '#FF0000',
    //         strokeOpacity: 1.0,
    //         strokeWeight: 2
    //       });
        
    //      // flightPath.setMap(map);

    // });
    let heatmap = new google.maps.visualization.HeatmapLayer({
        data: heatMapData
        // ,maxIntensity : 70

      });
      heatmap.setMap(map);

   
    // trips.forEach(t => {
    //     // t.coords.forEach(c => {
    //     //     let latLng = new google.maps.LatLng(c.lat,c.lng);
    //     //     let marker = new google.maps.Marker({
    //     //       position: latLng,
    //     //       map: map
    //     //     });

    //     // });

    //     var coords = t.coords[0]; // starting point
    //     var latLng = new google.maps.LatLng(coords.lat,coords.lng);
    //     var marker = new google.maps.Marker({
    //       position: latLng,
    //       map: map
    //     });

    // });

}

function consolidate_within_feet(data,miles){
    let averaged = [];
    data.forEach(coordDat =>{
        console.log("adding coord:", coordDat.tripNumber);
        addDataToAveraged(averaged,coordDat,miles);

    });
    return averaged;
}
function addDataToAveraged(average,newEntry,miles){
    let obj = average.find(x=> distance(x.lat,x.lng,newEntry.lat,newEntry.lng) < miles);
    if(obj == undefined){
        obj = {};
        obj.dataPoints = 1; //this first one.
        obj.lat = newEntry.lat;
        obj.lng = newEntry.lng;
        obj.aveSpeed = newEntry.speed;
        average.push(obj);
        console.log(average.length)
    }else{
        console.log("FOUND")
        obj.dataPoints+=1;
        obj.aveSpeed = obj.aveSpeed * ((obj.dataPoints -1)/obj.dataPoints) + newEntry.speed * (1/obj.dataPoints);
        
        
    }



}
function distance(lat1, lon1, lat2, lon2, unit) {
	var radlat1 = Math.PI * lat1/180
	var radlat2 = Math.PI * lat2/180
	var theta = lon1-lon2
	var radtheta = Math.PI * theta/180
	var dist = Math.sin(radlat1) * Math.sin(radlat2) + Math.cos(radlat1) * Math.cos(radlat2) * Math.cos(radtheta);
	dist = Math.acos(dist)
	dist = dist * 180/Math.PI
	dist = dist * 60 * 1.1515
	if (unit=="K") { dist = dist * 1.609344 }
	if (unit=="N") { dist = dist * 0.8684 }
	return dist
}

