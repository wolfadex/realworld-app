module Article exposing (Listing, updateListing)

import Conduit.Api


type alias Listing =
    { articles : List Conduit.Api.Article
    , page : Int
    , totalPages : Int
    }


updateListing : Conduit.Api.Article -> Listing -> Listing
updateListing article listing =
    let
        articles : List Conduit.Api.Article
        articles =
            List.map
                (\a ->
                    if a.slug == article.slug then
                        article

                    else
                        a
                )
                listing.articles
    in
    { listing | articles = articles }
