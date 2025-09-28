module ZeroOrMore exposing (ZeroOrMore(..), cons, insertOrReplace, member, remove, removeWhen, toggle)

import List.Extra


type ZeroOrMore a
    = None
    | One a
    | TwoOrMore a a (List a)


member : a -> ZeroOrMore a -> Bool
member m zom =
    case zom of
        None ->
            False

        One a ->
            m == a

        TwoOrMore a b c ->
            m == a || m == b || List.member m c


toggle : (a -> a -> Bool) -> Bool -> a -> ZeroOrMore a -> ZeroOrMore a
toggle isSame selected item list =
    if selected then
        insertOrReplace isSame item list

    else
        removeWhen (isSame item) list


removeWhen : (a -> Bool) -> ZeroOrMore a -> ZeroOrMore a
removeWhen isSame list =
    case list of
        None ->
            list

        One ex ->
            if isSame ex then
                None

            else
                list

        TwoOrMore a b c ->
            if isSame a then
                case c of
                    [] ->
                        One b

                    h :: t ->
                        TwoOrMore b h t

            else if isSame b then
                case c of
                    [] ->
                        One a

                    h :: t ->
                        TwoOrMore a h t

            else
                TwoOrMore a b (List.Extra.removeWhen isSame c)


remove : a -> ZeroOrMore a -> ZeroOrMore a
remove item list =
    case list of
        None ->
            list

        One ex ->
            if ex == item then
                None

            else
                list

        TwoOrMore a b c ->
            if a == item then
                case c of
                    [] ->
                        One b

                    h :: t ->
                        TwoOrMore b h t

            else if b == item then
                case c of
                    [] ->
                        One a

                    h :: t ->
                        TwoOrMore a h t

            else
                TwoOrMore a b (List.Extra.remove item c)


cons : a -> ZeroOrMore a -> ZeroOrMore a
cons h t =
    case t of
        None ->
            One h

        One e ->
            TwoOrMore h e []

        TwoOrMore a b c ->
            TwoOrMore h a (b :: c)


insertOrReplace : (a -> a -> Bool) -> a -> ZeroOrMore a -> ZeroOrMore a
insertOrReplace isSame item list =
    case list of
        None ->
            One item

        One ex ->
            if isSame item ex then
                One item

            else
                TwoOrMore ex item []

        TwoOrMore a b c ->
            if isSame item a then
                TwoOrMore item b c

            else if isSame item b then
                TwoOrMore a item c

            else
                TwoOrMore a b (item :: List.Extra.removeWhen (isSame item) c)
