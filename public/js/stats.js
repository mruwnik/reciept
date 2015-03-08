$(function () {
    $(".graph-limits #graph-start-date")
	.datepicker({ dateFormat: "dd/mm/yy",
		      onSelect: updateGraph});
    $(".graph-limits #graph-end-date")
	.datepicker({ dateFormat: "dd/mm/yy",
		      onSelect: updateGraph});

    updateGraph();

    $(".graph-scale input, .graph-limits input")
	    .bind('input', updateGraph);

    $('.graph-scale select').change(function(){
	updateGraph();
    })

});

var updateGraph = function(){
    $.getJSON( "/get-graph-data", 
	       {timestep: $(".graph-scale #timestep").val().trim(),
		timeunit: $('.graph-scale select[name="timestep-unit"]')
		                 .find(":selected").text().trim(),
		from: $(".graph-limits #graph-start-date").val()
       		       + " " + $(".graph-limits #graph-start-time").val(),
		to: $(".graph-limits #graph-end-date").val()
		       + " " + $(".graph-limits #graph-end-time").val()},
	       function( data ) {    
		   if(data){
		       $('#container').highcharts(data[0]);
		   }
	       });
};
