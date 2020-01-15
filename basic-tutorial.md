<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. CL-ODATA Basic Tutorial</a>
<ul>
<li><a href="#sec-1-1">1.1. Introduction</a></li>
<li><a href="#sec-1-2">1.2. Requesting Data</a></li>
<li><a href="#sec-1-3">1.3. Requesting an Individual Entity by ID</a></li>
<li><a href="#sec-1-4">1.4. Requesting an Individual Property</a></li>
<li><a href="#sec-1-5">1.5. Querying Data</a>
<ul>
<li><a href="#sec-1-5-1">1.5.1. Basic predicates, built-in functions</a></li>
<li><a href="#sec-1-5-2">1.5.2. Filter on Complex Type</a></li>
<li><a href="#sec-1-5-3">1.5.3. Filter on Enum Properties</a></li>
</ul>
</li>
</ul>
</li>
</ul>
</div>
</div>

# CL-ODATA Basic Tutorial<a id="sec-1" name="sec-1"></a>

This is CL-ODATA version of TripPin tutorial. See the original tutorial at: <https://www.odata.org/getting-started/basic-tutorial/>

## Introduction<a id="sec-1-1" name="sec-1-1"></a>

The Open Data Protocol (OData) is a data access protocol built on core protocols like HTTP and commonly accepted methodologies like REST for the web. There are various kinds of libraries and tools can be used to consume OData services. But for beginners and those who want to write their own libraries, the pure HTTP requests and responses are also very important. This documentation will not cover every feature details for OData V4 services but will try to cover various typical scenarios. If you want to have a more detailed understanding, please refer to OData Documentation.

## Requesting Data<a id="sec-1-2" name="sec-1-2"></a>

OData services support requests for data via HTTP GET requests.
Requesting Entity Collections

The request below returns the the collection of Person People.

    (-> +trip-pin-modify+
        (collection "People")
        (fetch :collection))

    (((:odata-id
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('russellwhyte')")
      (:odata-etag . "W/\"08D7983E303B2043\"")
      (:odata-edit-link
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('russellwhyte')")
      (:user-name . "russellwhyte") (:first-name . "Russell")
      (:last-name . "Whyte") (:emails "Russell@example.com" "Russell@contoso.com")
      (:address-info
       ((:address . "187 Suffolk Ln.")
        (:city (:country-region . "United States") (:name . "Boise")
         (:region . "ID"))))
      (:gender . "Male") (:concurrency . 637145265160790083))
     ((:odata-id
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('scottketchum')")
      (:odata-etag . "W/\"08D7983E303B2043\"")
      (:odata-edit-link
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('scottketchum')")
      (:user-name . "scottketchum") (:first-name . "Scott")
      (:last-name . "Ketchum") (:emails "Scott@example.com")
      (:address-info
       ((:address . "2817 Milton Dr.")
        (:city (:country-region . "United States") (:name . "Albuquerque")
         (:region . "NM"))))
      (:gender . "Male") (:concurrency . 637145265160790083))
     ((:odata-id
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('ronaldmundy')")
      (:odata-etag . "W/\"08D7983E303B2043\"")
      (:odata-edit-link
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('ronaldmundy')")
      (:user-name . "ronaldmundy") (:first-name . "Ronald") (:last-name . "Mundy")
      (:emails "Ronald@example.com" "Ronald@contoso.com") (:address-info)
      (:gender . "Male") (:concurrency . 637145265160790083))
     ((:odata-id
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('javieralfred')")
      (:odata-etag . "W/\"08D7983E303B2043\"")
      (:odata-edit-link
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('javieralfred')")
      (:user-name . "javieralfred") (:first-name . "Javier")
      (:last-name . "Alfred") (:emails "Javier@example.com" "Javier@contoso.com")
      (:address-info
       ((:address . "89 Jefferson Way Suite 2")
        (:city (:country-region . "United States") (:name . "Portland")
         (:region . "WA"))))
      (:gender . "Male") (:concurrency . 637145265160790083))
     ((:odata-id
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('willieashmore')")
      (:odata-etag . "W/\"08D7983E303B2043\"")
      (:odata-edit-link
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('willieashmore')")
      (:user-name . "willieashmore") (:first-name . "Willie")
      (:last-name . "Ashmore") (:emails "Willie@example.com" "Willie@contoso.com")
      (:address-info) (:gender . "Male") (:concurrency . 637145265160790083))
     ((:odata-id
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('vincentcalabrese')")
      (:odata-etag . "W/\"08D7983E303B2043\"")
      (:odata-edit-link
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('vincentcalabrese')")
      (:user-name . "vincentcalabrese") (:first-name . "Vincent")
      (:last-name . "Calabrese")
      (:emails "Vincent@example.com" "Vincent@contoso.com")
      (:address-info
       ((:address . "55 Grizzly Peak Rd.")
        (:city (:country-region . "United States") (:name . "Butte")
         (:region . "MT"))))
      (:gender . "Male") (:concurrency . 637145265160790083))
     ((:odata-id
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('clydeguess')")
      (:odata-etag . "W/\"08D7983E303B2043\"")
      (:odata-edit-link
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('clydeguess')")
      (:user-name . "clydeguess") (:first-name . "Clyde") (:last-name . "Guess")
      (:emails "Clyde@example.com") (:address-info) (:gender . "Male")
      (:concurrency . 637145265160790083))
     ((:odata-id
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('keithpinckney')")
      (:odata-etag . "W/\"08D7983E303B2043\"")
      (:odata-edit-link
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('keithpinckney')")
      (:user-name . "keithpinckney") (:first-name . "Keith")
      (:last-name . "Pinckney") (:emails "Keith@example.com" "Keith@contoso.com")
      (:address-info) (:gender . "Male") (:concurrency . 637145265160790083)))

## Requesting an Individual Entity by ID<a id="sec-1-3" name="sec-1-3"></a>

The request below returns an individual entity of type Person by the given UserName "russellwhyte"

    (-> +trip-pin-modify+
        (collection "People")
        (id "russellwhyte")
        (fetch))

    ((:odata-context
      . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/$metadata#People/$entity")
     (:odata-id
      . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('russellwhyte')")
     (:odata-etag . "W/\"08D7983E303B2043\"")
     (:odata-edit-link
      . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('russellwhyte')")
     (:user-name . "russellwhyte") (:first-name . "Russell") (:last-name . "Whyte")
     (:emails "Russell@example.com" "Russell@contoso.com")
     (:address-info
      ((:address . "187 Suffolk Ln.")
       (:city (:country-region . "United States") (:name . "Boise")
        (:region . "ID"))))
     (:gender . "Male") (:concurrency . 637145265160790083))

## Requesting an Individual Property<a id="sec-1-4" name="sec-1-4"></a>

To address an entity property clients append a path segment containing property name to the URL of the entity. If the property has a complex type, properties of that value can be addressed by further property name composition.
First let's take a look at how to get a simple property. The request below returns the Name property of an Airport.

    (-> +trip-pin-modify+
        (collection "Airports")
        (id "KSFO")
        (property "Name")
        (fetch :value))

    "San Francisco International Airport"

Then let's see how to get a property value of a complex type. The request below returns the Address of the complex type Location in an Airport.

    (-> +trip-pin-modify+
        (collection "Airports")
        (id "KSFO")
        (property "Location")
        (property "Address")
        (fetch :value))

    "South McDonnell Road, San Francisco, CA 94128"

## Querying Data<a id="sec-1-5" name="sec-1-5"></a>

OData supports various kinds of query options for querying data. This section will help you go through the common scenarios for these query options.
System Query Option $filter

The $filter system query option allows clients to filter a collection of resources that are addressed by a request URL. The expression specified with $filter is evaluated for each resource in the collection, and only items where the expression evaluates to true are included in the response.

### Basic predicates, built-in functions<a id="sec-1-5-1" name="sec-1-5-1"></a>

There are several kinds of basic predicates and built-in functions for $filter, including logical operators and arithmetic operators. For more detailed information, please refer to OData V4 URL Conventions Document. The request below using $filter to get people with FirstName "Scott".

Just use raw expressions as $filter input.

    (-> +trip-pin-modify+
        (collection "People")
        ($filter "FirstName eq 'Scott'")
        (fetch :collection))

    (((:odata-id
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('scottketchum')")
      (:odata-etag . "W/\"08D7983E303B2043\"")
      (:odata-edit-link
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('scottketchum')")
      (:user-name . "scottketchum") (:first-name . "Scott")
      (:last-name . "Ketchum") (:emails "Scott@example.com")
      (:address-info
       ((:address . "2817 Milton Dr.")
        (:city (:country-region . "United States") (:name . "Albuquerque")
         (:region . "NM"))))
      (:gender . "Male") (:concurrency . 637145265160790083)))

Or Lisp based expressions:

    (-> +trip-pin-modify+
        (collection "People")
        ($filter '(:= "FirstName" "Scott"))
        (fetch :collection))

    (((:odata-id
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('scottketchum')")
      (:odata-etag . "W/\"08D7983E303B2043\"")
      (:odata-edit-link
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('scottketchum')")
      (:user-name . "scottketchum") (:first-name . "Scott")
      (:last-name . "Ketchum") (:emails "Scott@example.com")
      (:address-info
       ((:address . "2817 Milton Dr.")
        (:city (:country-region . "United States") (:name . "Albuquerque")
         (:region . "NM"))))
      (:gender . "Male") (:concurrency . 637145265160790083)))

### Filter on Complex Type<a id="sec-1-5-2" name="sec-1-5-2"></a>

$filter can also work on complex type. The request below returns airports with "San Francisco" contained in its Address. And Address is property of complex type Location.

    (-> +trip-pin-modify+
        (collection "Airports")
        ($filter "contains(Location/Address, 'San Francisco')")
        (fetch :collection))

    (((:odata-id
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/Airports('KSFO')")
      (:odata-edit-link
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/Airports('KSFO')")
      (:icao-code . "KSFO") (:name . "San Francisco International Airport")
      (:iata-code . "SFO")
      (:location (:address . "South McDonnell Road, San Francisco, CA 94128")
       (:city (:country-region . "United States") (:name . "San Francisco")
        (:region . "California"))
       (:loc (:type . "Point") (:coordinates -122.374725 37.61889)
        (:crs (:type . "name") (:properties (:name . "EPSG:4326")))))))

    (-> +trip-pin-modify+
        (collection "Airports")
        ($filter '(:contains "Location/Address" "San Francisco"))
        (fetch :collection))

    (((:odata-id
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/Airports('KSFO')")
      (:odata-edit-link
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/Airports('KSFO')")
      (:icao-code . "KSFO") (:name . "San Francisco International Airport")
      (:iata-code . "SFO")
      (:location (:address . "South McDonnell Road, San Francisco, CA 94128")
       (:city (:country-region . "United States") (:name . "San Francisco")
        (:region . "California"))
       (:loc (:type . "Point") (:coordinates -122.374725 37.61889)
        (:crs (:type . "name") (:properties (:name . "EPSG:4326")))))))

### Filter on Enum Properties<a id="sec-1-5-3" name="sec-1-5-3"></a>

The request below returns all female People of entity type Person. The Gender is a property of Enum type.

    (-> +trip-pin-modify+
        (collection "People")
        ($filter `(:eq "Gender" ,+person-gender/female+))
        (fetch :collection))

    (((:odata-id
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('elainestewart')")
      (:odata-etag . "W/\"08D7983E303B2043\"")
      (:odata-edit-link
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('elainestewart')")
      (:user-name . "elainestewart") (:first-name . "Elaine")
      (:last-name . "Stewart") (:emails "Elaine@example.com" "Elaine@contoso.com")
      (:address-info) (:gender . "Female") (:concurrency . 637145265160790083))
     ((:odata-id
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('salliesampson')")
      (:odata-etag . "W/\"08D7983E303B2043\"")
      (:odata-edit-link
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('salliesampson')")
      (:user-name . "salliesampson") (:first-name . "Sallie")
      (:last-name . "Sampson") (:emails "Sallie@example.com" "Sallie@contoso.com")
      (:address-info
       ((:address . "87 Polk St. Suite 5")
        (:city (:country-region . "United States") (:name . "San Francisco")
         (:region . "CA")))
       ((:address . "89 Chiaroscuro Rd.")
        (:city (:country-region . "United States") (:name . "Portland")
         (:region . "OR"))))
      (:gender . "Female") (:concurrency . 637145265160790083))
     ((:odata-id
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('jonirosales')")
      (:odata-etag . "W/\"08D7983E303B2043\"")
      (:odata-edit-link
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('jonirosales')")
      (:user-name . "jonirosales") (:first-name . "Joni") (:last-name . "Rosales")
      (:emails "Joni@example.com" "Joni@contoso.com") (:address-info)
      (:gender . "Female") (:concurrency . 637145265160790083))
     ((:odata-id
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('georginabarlow')")
      (:odata-etag . "W/\"08D7983E303B2043\"")
      (:odata-edit-link
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('georginabarlow')")
      (:user-name . "georginabarlow") (:first-name . "Georgina")
      (:last-name . "Barlow")
      (:emails "Georgina@example.com" "Georgina@contoso.com") (:address-info)
      (:gender . "Female") (:concurrency . 637145265160790083))
     ((:odata-id
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('angelhuffman')")
      (:odata-etag . "W/\"08D7983E303B2043\"")
      (:odata-edit-link
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('angelhuffman')")
      (:user-name . "angelhuffman") (:first-name . "Angel")
      (:last-name . "Huffman") (:emails "Angel@example.com") (:address-info)
      (:gender . "Female") (:concurrency . 637145265160790083))
     ((:odata-id
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('laurelosborn')")
      (:odata-etag . "W/\"08D7983E303B2043\"")
      (:odata-edit-link
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('laurelosborn')")
      (:user-name . "laurelosborn") (:first-name . "Laurel")
      (:last-name . "Osborn") (:emails "Laurel@example.com" "Laurel@contoso.com")
      (:address-info) (:gender . "Female") (:concurrency . 637145265160790083))
     ((:odata-id
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('sandyosborn')")
      (:odata-etag . "W/\"08D7983E303B2043\"")
      (:odata-edit-link
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('sandyosborn')")
      (:user-name . "sandyosborn") (:first-name . "Sandy") (:last-name . "Osborn")
      (:emails "Sandy@example.com" "Sandy@contoso.com") (:address-info)
      (:gender . "Female") (:concurrency . 637145265160790083))
     ((:odata-id
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('ursulabright')")
      (:odata-etag . "W/\"08D7983E303B2043\"")
      (:odata-edit-link
       . "http://services.odata.org/V4/(S(wgkqtxlasrgtwhbtgoqbxc1l))/TripPinServiceRW/People('ursulabright')")
      (:user-name . "ursulabright") (:first-name . "Ursula")
      (:last-name . "Bright") (:emails "Ursula@example.com" "Ursula@contoso.com")
      (:address-info) (:gender . "Female") (:concurrency . 637145265160790083)))