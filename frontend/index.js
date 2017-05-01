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
  });
})();
//hello
