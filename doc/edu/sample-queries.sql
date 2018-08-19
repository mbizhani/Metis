select cae.id, cae.c_code, cae.c_name, ca.c_code g_code
from t_category ca
join t_category_element cae on cae.f_category = ca.id
;


select
    bk.id,
    bk.c_title,
    bk.n_publish_year,
    bk.n_price,
    bk.f_size,
    cae.c_name c_size,
    LISTAGG(au.c_name, ', ') WITHIN GROUP (ORDER BY au.c_name) authors
from t_book bk
join t_category_element cae on cae.id = bk.f_size
join mt_book_author mtb on mtb.f_book = bk.id
join t_person au on au.id = mtb.f_author
group by
    bk.id,
    bk.c_title,
    bk.n_publish_year,
    bk.n_price,
    bk.f_size,
    cae.c_name
;

select
    lc.id,
    lc.c_name,
    lc.f_type,
    cae.c_name loc_type,
    lc.f_parent
from t_location lc
join t_category_element cae on cae.id = lc.f_type
;

select
    pr.id,
    pr.c_name,
    pr.d_birth_date,
    pr.b_alive
from t_person pr
where
    1=1
--<# if (is_author??) #>
    and :is_author = :is_author
    and pr.id
--<# if (!is_author) #>
    not
--</#if>
    in (
        select f_author from mt_book_author
    )
--</#if>
;

select
    inv.id,
    st.c_name,
    inv.n_count,
    inv.f_book,
    bk.c_title
from t_inventory inv
join t_store st on st.id = inv.f_store
join t_book bk on bk.id = inv.f_book
