$(function () {
    $('.stats-container .change-currency select[name="currency"]')
	.change(function(e){
	      $.getJSON("/spending-stats",
			{currency: $("option:selected", this).val()},
	      function(data){
		  data = data[0];
		  $('.stats-container .spending-info')
		      .html(Mustache.render(headerStatsTemplate, 
					    {"values": data.values}));
	      });
	});


    jQuery.validator.addMethod("time", function(value, element) {
	return /^[012]\d?:[012345]\d$/.test(value);
    }, "Please give a correct time");

    $("#add-reciept-form").validate();

    $("#reciept-date").datepicker({ dateFormat: "dd/mm/yy"});

    $('.cost input[name="amount"]')
	    .bind('input', updateCost);

    var id = 1;
    $("#add-cost").click(function(){
	var desc = $('.cost [name="description"]');
	var cost = $('.cost [name="amount"]');
	var groups = $('.group-checkboxes input[type="checkbox"]:checked');
	groups = groups.map(function(i){return groups[i].value;})
	    .toArray().join(", ");
	if($('.cost input[name="new-groups"]').val()){
	    groups += (groups ? ", " : "");
	    groups += $('.cost input[name="new-groups"]').val();
	}

	$("#reciept-costs").html($("#reciept-costs").html()
		+ Mustache.render(addCostTemplate, 
				 {"desc": desc.val(),
				 "cost": cost.val(),
				 "groups": groups,
				 "id": id}));
	$('.reciept-cost input[name="amount' + id +'"]')
	    .bind('input', updateCost);
	id++;

	desc.val("");
	cost.val("");
	var groups = $('.group-checkboxes input[type="checkbox"]:checked'); 
	groups.map(function(i){groups[i].checked=false;});
	$('.cost input[name="new-groups"]').val("");
    });

    $('.reciepts .reciept-data').click(function(e){
	$(this).parent().toggleClass("active");
    });

    $(".tabs-container .tab").click(function(e){
	selectTab(this.id);
    });

    $('.costs table th a').click(setupSortCosts);
    $('.tabs-contents a.delete').click(deleteClick);


    $(".reciepts .costs .cost #edit-cost")
	.click(function(e){
	    var cost = $(this).parent().parent();
	    $('.edit-cost input[name="description"]', cost).val(
		$(".cost-data .description", cost).text().trim());
	    $('.edit-cost input[name="amount"]', cost).val(
		-parseInt($(".cost-data .amount", cost).text().trim()));

	    var groups = $(".cost-data .groups", cost)
		.text().trim().split(", ");
	    $('.group-checkboxes input[type="checkbox"]', cost)
		.map(function(i, cb){
		    if(0 <= $.inArray(cb.value, groups)){
			cb.checked = true;
		    }
		}); 

	    cost.toggleClass("edit-mode");
	});

    $(".reciepts .costs .cost #cancel")
	.click(function(e){
	    var cost = $(this).parent().parent();
	    cost.toggleClass("edit-mode");
	});
    
    $(".reciepts .costs .cost #save-cost")
	.click(function(e){
	    var instance = this;
	    var parent = $(this).parent();
	    
	    var ajaxData = {id: $('input[name="id"]', parent).val(),
			    "reciept-id": $('input[name="reciept-id"]', parent).val(),
			    description:  $('input[name="description"]',
					    parent).val(),
			    amount: $('input[name="amount"]', parent).val(),
			    "new-groups": $('input[name="new-groups"]',
					    parent).val(),
			    ajax: true
			   }; 
	    
	    var groups = "";
	    $('.group-checkboxes input[type="checkbox"]:checked', parent)
		.map(function(i, cb){
		    ajaxData["groups" + i] = cb.value;
		    if(!("NONE".toUpperCase() === cb.value.toUpperCase())){
			groups += (groups ? ", " : "") + cb. value;
		    }
		});
	    if(ajaxData["new-groups"]){
		groups += ajaxData["new-groups"];
	    }

	    $.post("/add-cost", ajaxData,
		 function(data) {
		     parent = parent.parent();
		     parent.toggleClass("edit-mode");
		     
		     $(".cost-data .description", parent)
			 .text(ajaxData.description);
		     $(".cost-data .amount", parent)
			 .text(-parseInt(ajaxData.amount));
		     
		     $(".cost-data .groups", parent)
			 .text(groups ? groups : "NONE");
		     
		     updateAmount(parent.parent().parent());
		     if(data[0].amount){
			 $("#amount-" + data[0].amount["currency"])
			     .text(data[0].amount["total-amount"]);
		     }
		 }, "json");
	});
	    
});

var setupSortCosts = function(e) {
	e.preventDefault();

	var dataUrl = "";
	$.ajax({url: this.href})
	    .done(function(data) {
		$('#costs-table').html(data);
		$('#costs-table table th a').click(setupSortCosts);
		$('.tabs-contents a.delete').click(deleteClick);
	   });
	
	return false;
    }

var deleteClick = function(e) {
    var instance = this;
    e.preventDefault();
    $.getJSON(this.href, {ajax: true},
	      function(data){
		  data = data[0];
		  if(data.costs){
		      for(var i = 0; i < data.costs.length; i++){
			  $('[id="cost' + data.costs[i] + '"]').remove();
		      }
		  }
		  if(data.reciepts){
		      for(var i = 0; i < data.reciepts.length; i++){
			  $('[id="reciept' + data.reciepts[i] + '"]')
			      .remove();
		      }
		  }
		  $("#undo-link").removeClass("hidden");
		  if(data.amount){
		      $("#amount-" + data.amount["currency"])
			  .text(data.amount["total-amount"]);
		      
		      if(data.amount.parentId){
			  updateAmount("#reciept" + data.amount.parentId);		  
		      }
		  }
	      });
    return false;
    }

var updateAmount = function(reciept){
    var sum = 0; 
    var costs = $(".costs .amount", reciept);
    costs.map(function(i, value){
	sum += parseInt(value.innerHTML.trim())
    });
    $(".reciept-data .amount", reciept)
	.text(sum + " " + "asd");//data.amount.currency.toUpperCase());
};

var updateCost = function(){
    var costs = $('input[type="text"].cost-amount');
    var cost = 0
    for(var i = 0; i < costs.length; i++){
	cost += parseInt(costs[i].value);
    }
    $(".reciept-header #reciept-amount").text(cost);
};

var selectTab = function(tabid){
    $(".tabs-container .tab, .tabs-container .tab-contents")
	.removeClass("active");
    $(".tabs-container #" + tabid 
      + ", .tabs-container #" + tabid + "-contents")
	.addClass("active");
}
