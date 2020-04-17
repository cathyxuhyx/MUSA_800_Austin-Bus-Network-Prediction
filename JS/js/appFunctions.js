/* ================================
Week 6 Assignment: Midterm Functions + Signatures
================================ */
//Leaflet Configuration
var map = L.map('map', {
  center: [30.295606, -97.741084],
  zoom: 11
});

var Stamen_TonerLite = L.tileLayer('http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png', {
  attribution: 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>',
  subdomains: 'abcd',
  minZoom: 0,
  maxZoom: 20,
  ext: 'png'
}).addTo(map);

//define variables and dataset links
var dataset = "../js_test.csv";
var markers;

//define colors or size of the individual marker with respect to covid 19 cases
var myStyle = function(row) {};

//function to plot the locations
var makeMarkers = function (data) {
  addmarker = _.map(_.rest(data), function (row) {
    lat = Number(row[5]);
    lng = Number(row[6]);
    if (!isNaN(lat) && !isNaN(lng)) {
      return L.circleMarker([lat, lng], myStyle(row));}
});
  return addmarker;
};

// and puts them on the map
var plotMarkers = function (marker) {
  _.each(marker, function (x){
    if (typeof(x) !== "undefined") {return x.addTo(map); }
  });
};

// Remove markers
var removeMarkers = function (marker) {
  _.each(marker, function (x){
    if (typeof(x) !== "undefined") {return map.removeLayer(x);}
  });
};

//show results
var showResults = function() {
  $('#intro').hide();
  $('#USonly').hide();
  // => <div id="results">
  $('#results').show();
};

//close results and return to original state
var closeResults = function() {
  $('#intro').show();
  $('#USonly').hide();
  $('#results').hide();
  map.setView([38.8, -97.6129], 4);
};

//change side bar information with respect to each country
var eachFeatureFunction = function(marker) {
  if (typeof(marker) != "undefined") {
    marker.on('click', function(event) {
    });
  }
};

//run the analysis by start the request of the dataset
$(document).ready(function() {
  $.ajax(dataset).done(function(data) {

    //parse the csv file
    var rows = data.split("\n");
    worlddata = [];
    for (var i=0;i<rows.length;i=i+1){
        worlddata.push(rows[i].split(','));}
    filtered_worlddata = _.filter(worlddata, function(row){
      return Number(row[7])>0;});

    //make markers and plot them
    markers = makeMarkers(filtered_worlddata);
    // find non-US markers
    var realmarkers = _.filter(markers, function(marker){
      return typeof(marker) != "undefined";});
    plotMarkers(realmarkers);

    //click event for each marker
    _.each(markers, function(marker){eachFeatureFunction(marker);});

    //see the highest riderships

  });
});
$("button").click(function() {closeResults();});
