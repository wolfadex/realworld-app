module Article exposing (Listing, updateListing)

import Conduit.Types


type alias Listing =
    { articles : List Conduit.Types.Article
    , page : Int
    , totalPages : Int
    }


updateListing : Conduit.Types.Article -> Listing -> Listing
updateListing article listing =
    let
        articles : List Conduit.Types.Article
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
