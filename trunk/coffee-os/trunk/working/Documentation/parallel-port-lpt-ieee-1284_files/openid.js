// $Id: openid.js,v 1.1.2.3 2007/05/24 15:01:21 walkah Exp $

$(document).ready(
    function() {
    $("a.openid-link").click( function() {
      $("#edit-pass-wrapper").hide();
      $("#edit-name-wrapper").fadeOut('medium', function() {
        $("#edit-openid-url-wrapper").fadeIn('medium');
        } );
      $("a.openid-link").hide();
      return false;
      } );
    } );

