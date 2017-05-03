'use strict';
(function() {
  var handler = null;

  $(document).ready(function() {
    handler = StripeCheckout.configure({
      key: 'pk_test_AEzcSG6wdn2OKEGPmMVZSWcK',
      image: 'https://stripe.com/img/documentation/checkout/marketplace.png',
      locale: 'auto', // TODO change to en_CA
      token: function(token) {
        // TODO sending token server-side happens here
      },
    });

    $('#submit').click(function() {
      handler.open({
        name: 'McGill Book Renewal',
        description: '4 months of daily book renewals',
        currency: 'cad',
        amount: 100,
      });
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

    $('#see-my-books').click(function() {
      // * make username-and-password-input disappear
      // * replace it with a spinner
      // * perform ajax to register the user with the current username and
      //   password
      // * if the response comes back ok, then we get a list of books, so we
      //   create a table showing the user that information
      // * if the response comes back in error, then show the text input fields
      //   again and display "username/password" incorrect

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
      if(hasErrors) return;

      $('#username-and-password-input input').prop('disabled', true);
      $('#progress').show();
    });
  });
})();

// vim: shiftwidth=2 tw=79
