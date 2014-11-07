$(function () {
    $.getJSON( "/get-graph-data", function( data ) {    
	$('#container').highcharts(data[0]);
    });
});
