'use strict';
(function() {
  var handler = null;

  $(document).ready(function() {
    var progress = $('#progress');

    function showProgress() {
      progress.css('visibility', 'visible');
    }

    function hideProgress() {
      progress.css('visibility', 'hidden');
    }

    handler = StripeCheckout.configure({
      key: 'pk_test_AEzcSG6wdn2OKEGPmMVZSWcK',
      image: 'https://stripe.com/img/documentation/checkout/marketplace.png',
      locale: 'auto', // TODO change to en_CA
      token: function(token) {
        showProgress();

        postPayment(
          {
            tokenId: token.id,
            paymentUsername: $('#username-input').val(),
          },
          function() {
            hideProgress();
            alert('yay');
          },
          function() {
            hideProgress();
            clearValidationErrors();
            $(validationErrorsNode).append(
              '<li> An error occurred processing your payment. Don\'t worry, nothing has been charged to your card! </li>'
            );
          }
        );
      },
    });

    $('#submit').click(function() {
      registerUser(
        function(profile) {
          handler.open({
            name: 'McGill Book Renewal',
            description: '4 months of daily book renewals',
            currency: 'cad',
            amount: 100,
          });
        },
        function(errors) {
          clearValidationErrors();
          $(validationErrorsNode).append(
            '<li> We could not verify your account. Please double-check your username and password. </li>'
          );
        }
      );
    });

    window.addEventListener('popstate', function() {
      handler.close();
    });

    $('#signup-button').click(function() {
        $('#signup-button').hide();
        console.log('hi');
        $('#signup-card').fadeIn(400, function() {console.log('done');});
    });

    function validateEmail(email) {
      var re = /^(([^<>()\[\]\\.,;:\s@"]+(\.[^<>()\[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;
      return re.test(email);
    }

    function validateUsername() {
      var username = $('#username-input');
      return username.val() && validateEmail(username.val());
    }

    function validatePassword() {
      return $('#password-input').val();
    }

    var validationErrorsNode = document.getElementById('validation-errors');

    function clearValidationErrors() {
      while(validationErrorsNode.hasChildNodes()) {
        validationErrorsNode.removeChild(validationErrorsNode.lastChild);
        console.log('Removed error.');
      }
    }

    function validateInputs() {
      clearValidationErrors();
      var hasErrors = false;
      if(!validateUsername()) {
        $(validationErrorsNode).append(
          $("<li> Please provide a valid McGill email address. </li>")
        );
        hasErrors = true;
        console.log('added username error');
      }
      if(!validatePassword()) {
        $(validationErrorsNode).append(
          $("<li> Please provide the password. </li>")
        );
        hasErrors = true;
        console.log('added password error');
      }
      return !hasErrors;
    }

    function registerUser(success, fail) {
      if(!validateInputs()) return;

      $('#username-and-password-input input').prop('disabled', true);
      console.log('inputs are disabled');
      showProgress();

      postRegister(
        {
          registrantUsername: $('#username-input').val(),
          notificationEmail: '',
          pass: $('#password-input').val(),
          phoneNumber: '',
          trigger: 'onlyFailure'
        },
        function(profile) {
          hideProgress();
          $('#username-and-password-input input').prop('disabled', false);
          success(profile);
        },
        function(errors) {
          hideProgress();
          $('#username-and-password-input input').prop('disabled', false);
          fail(errors);
        }
      );
    }

    $('#see-my-books').click(function() {
      registerUser(
        function(profile) {
          console.log('it worked');
          console.log(JSON.stringify(profile));
          constructTable(profile.detailedBookList);
        },
        function(errors) {
          clearValidationErrors();
          $(validationErrorsNode).append(
            $("<li> An error occurred while fetching your books. </li>")
          );
          console.log('errors ' + JSON.stringify(errors));
        }
      );
    });

    var table = $('#table');

    function clearTable() {
      table.children('td').remove();
    }

    function constructTable(books) {
      clearTable();
      books.forEach(function(book) {
        var tr = $('<tr></tr>');
        tr.append($('<td></td>').text(book.description));
        tr.append($('<td></td>').text(book.library));
        tr.append($('<td></td>').text(book.accruingFine));
        tr.append($('<td></td>').text(book.dueDate + ' ' + book.dueHour));
        tr.append($('<td></td>').text(book.year));
        tr.append($('<td></td>').text(book.numberOfRenewals));
        table.append(tr);
      });
      table.show();
    }

    function onUnload() {
      $('#username-and-password-input input').prop('disabled', false);
    }

    window.addEventListener('unload', onUnload, false);


    $('#username-and-password-input input').prop('disabled', false);
  });
})();

// vim: shiftwidth=2 tw=79
