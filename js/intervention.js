// var modal = document.getElementById("myModal");
// var span = document.getElementsByClassName("closemod")[0];

// console.log(modal);
// function openModal(button) {
//   var htmlContent = button.parentNode.previousElementSibling.innerHTML;
//   modal.style.display = "block";
//   document.getElementById("modalText").innerHTML = htmlContent;
// }

// function closeModal() {
//   modal.style.display = "none";
// }

// window.onclick = function (event) {
//   if (event.target == modal) {
//     modal.style.display = "none";
//   }
// };

$(document).ready(function () {
  $(".js-example-basic-multiple").select2({});

  // Populate the first filter with unique values from the table
  $("#data-table").load(
    "../interventions/interventions_list.html",
    function () {
      var typeOptions = ["Psychosocial", "Complementary", "Pharmacological"];
      $.each(typeOptions, function (i, value) {
        $("#filter1").append(
          '<option value="' + value + '">' + value + "</option>"
        );
      });

      var ageOptions = [
        "Very young children",
        "School-age children",
        "Adolescents",
        "Adults",
      ];
      $.each(ageOptions, function (i, value) {
        $("#filter2").append(
          '<option value="' + value + '">' + value + "</option>"
        );
      });

      var outcomeOptions = [
        {
          group: "Key outcomes",
          options: [
            "Core ASD symptoms",
            "Safety",
            // "Overall ASD symptoms",
            // "Social communication",
            // "Restricted repetitive behaviors",
          ],
        },
        {
          group: "Associated symptoms",
          options: [
            "Problematic behaviors",
            "Psychiatric comorbidity",
            "Language",
          ],
        },
        {
          group: "Daily life variables",
          options: ["Functioning", "Sleep"],
        },
      ];

      $.each(outcomeOptions, function (i, group) {
        $("#filter3").append('<optgroup label="' + group.group + '">');

        $.each(group.options, function (j, option) {
          $("#filter3").append(
            '<option value="' + option + '">' + option + "</option>"
          );
        });

        $("#filter3").append("</optgroup>");
      });
    }
  );

  // Event listener for filter1 (type of intervention)
  $("#filter1").on("change", function () {
    filterTable();
  });
  $("#filter2").on("change", function () {
    filterTable();
  });
  $("#filter3").on("change", function () {
    filterTable();
  });

  // Function to filter the table based on both filters
  function filterTable() {
    var typeFilter = $("#filter1").val() || [];
    var ageFilter = $("#filter2").val() || [];
    var outFilter = $("#filter3").val() || [];
    $("#data-table tbody tr").each(function () {
      var type = $(this).find("td:eq(0)").text();
      var age = $(this).find("td:eq(2)").text();
      var out = $(this).find("td:eq(3)").text();
      var showRow = true;
      // if (typeFilter.length > 0 && typeFilter.indexOf(type) === -1) {
      //   showRow = false;
      // }
      if (typeFilter.length > 0) {
        var typeMatched = false;
        typeFilter.forEach(function (typeFilter) {
          if (type.includes(typeFilter)) {
            typeMatched = true;
          }
        });
        if (!typeMatched) {
          showRow = false;
        }
      }
      if (ageFilter.length > 0) {
        var ageMatched = false;
        ageFilter.forEach(function (filterAge) {
          if (age.includes(filterAge)) {
            ageMatched = true;
          }
        });
        if (!ageMatched) {
          showRow = false;
        }
      }
      if (outFilter.length > 0) {
        var outMatched = false;
        outFilter.forEach(function (filterOut) {
          if (out.includes(filterOut)) {
            outMatched = true;
          }
        });
        if (!outMatched) {
          showRow = false;
        }
      }
      if (showRow) {
        $(this).show();
      } else {
        $(this).hide();
      }
    });
  }
});
// Themes begin
// am4core.useTheme(am4themes_animated);
// Themes end

// var chart = am4core.create("chartdiv", am4plugins_wordCloud.WordCloud);
// var series = chart.series.push(new am4plugins_wordCloud.WordCloudSeries());
// chart.logo.disabled = true;
// series.randomness = 0;
// series.accuracy = 1;
// series.step = 50;
// series.maxCount = 150;
// series.minWordLength = 1;
// series.labels.template.tooltipText = "{word} (n clinical trial = {value})";
// // series.fontFamily = "Courier New";
// series.maxFontSize = am4core.percent(10);

// series.text =
//   "EIBI, EIBI, EIBI, EIBI, EIBI, EIBI, EIBI, EIBI, EIBI, EIBI, EIBI, EIBI, EIBI, EIBI, EIBI, EIBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, NDBI, DEV, DEV, DEV, DEV, DEV, DEV, DEV, DEV, DEV, DEV, DEV, DEV, DEV, DEV, DEV, SSG, SSG, SSG, SSG, SSG, SSG, SSG, SSG, SSG, SSG, SSG, SSG, SSG, SSG, SSG, SSG, SSG, SSG, SSG, SSG, SSG, SSG, SSG, SSG, SSG, SSG, SSG, SSG, SSG, SSG, SSG, SSG, SSG, SSG, SSG, SSG, SSG, SSG, SSG, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, PMI, CBT, CBT, CBT, CBT, CBT, CBT, TECH, TECH, TECH, TECH, TECH, TECH, TECH, TECH, TECH, TECH, TECH, TECH, TECH, TECH, TECH, TECH, TECH, TECH, TECH, TECH, TECH, TECH, TECH, TECH, TECH, TECH, TECH, TECH, TECH, TECH, TECH, TECH, TECH, TECH, TECH, TECH, TEACCH, TEACCH, TEACCH, TEACCH, TEACCH, TEACCH, TEACCH, TEACCH, TEACCH, TEACCH, AAI, ACUP, AIT, AMIN, DIET, DIET, DIET, DIET, MUSIC, OXYT, PHYS, PHYS, PHYS, PHYS, PHYS, PHYS, PHYS, PHYS, PHYS, PHYS, PHYS, PHYS, PHYS, PHYS, PHYS, PHYS, PHYS, PHYS, PHYS, PHYS, PHYS, PHYS, PHYS, PHYS, PHYS, PHYS, PHYS, PHYS, PHYS, PHYS, PHYS, PUFA";
