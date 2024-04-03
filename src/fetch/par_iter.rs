use rayon::iter::plumbing::UnindexedConsumer;
use rayon::prelude::*;

use super::*;

/// A [`ParallelIterator`] over entities matching the query `Q`.
///
/// This is the parallel version of [`Iter`].
#[must_use = "iterators are lazy and do nothing unless consumed"]
pub struct ParIter<'a, Q: Query> {
    indices: &'a [ArchetypeIdx],
    states: &'a [Q::ArchState],
    archetypes: &'a Archetypes,
}

impl<'a, Q: Query> ParIter<'a, Q> {
    pub(super) unsafe fn new(
        indices: &'a [ArchetypeIdx],
        states: &'a [Q::ArchState],
        archetypes: &'a Archetypes,
    ) -> Self {
        Self {
            indices,
            states,
            archetypes,
        }
    }
}

impl<Q: ReadOnlyQuery> Clone for ParIter<'_, Q> {
    fn clone(&self) -> Self {
        Self {
            arch_states: self.arch_states,
            arch_indices: self.arch_indices,
            archetypes: self.archetypes,
        }
    }
}

impl<Q: Query> fmt::Debug for ParIter<'_, Q> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ParIter")
            .field("indices", &self.indices)
            .field("states", &self.states)
            .field("archetypes", &self.archetypes)
            .finish()
    }
}

impl<'a, Q> ParallelIterator for ParIter<'a, Q>
where
    Q: Query,
    Q::Item<'a>: Send,
{
    type Item = Q::Item<'a>;

    fn drive_unindexed<C>(self, consumer: C) -> C::Result
    where
        C: UnindexedConsumer<Self::Item>,
    {
        unsafe { assume_debug_checked(self.arch_states.len() == self.arch_indices.len()) };

        self.arch_states
            .par_iter()
            .zip_eq(self.arch_indices)
            .flat_map(|(state, &index)| {
                let entity_count =
                    unsafe { self.archetypes.get(index).unwrap_debug_checked() }.entity_count();

                (0..entity_count).into_par_iter().map(|row| {
                    let item: Q::Item<'a> = unsafe { Q::get(state, ArchetypeRow(row)) };
                    item
                })
            })
            .drive_unindexed(consumer)
    }
}

#[cfg_attr(docsrs, doc(cfg(feature = "rayon")))]
impl<'a, Q> IntoParallelIterator for Fetcher<'a, Q>
where
    Q: Query,
    Q::Item<'a>: Send,
{
    type Iter = ParIter<'a, Q>;

    type Item = Q::Item<'a>;

    fn into_par_iter(self) -> Self::Iter {
        unsafe { self.state.par_iter_mut(self.world.archetypes()) }
    }
}

#[cfg_attr(docsrs, doc(cfg(feature = "rayon")))]
impl<'a, Q> IntoParallelIterator for &'a Fetcher<'_, Q>
where
    Q: ReadOnlyQuery,
    Q::Item<'a>: Send,
{
    type Iter = ParIter<'a, Q>;

    type Item = Q::Item<'a>;

    fn into_par_iter(self) -> Self::Iter {
        unsafe { self.state.par_iter(self.world.archetypes()) }
    }
}

#[cfg_attr(docsrs, doc(cfg(feature = "rayon")))]
impl<'a, Q: Query> IntoParallelIterator for &'a mut Fetcher<'_, Q>
where
    Q: Query,
    Q::Item<'a>: Send,
{
    type Iter = ParIter<'a, Q>;

    type Item = Q::Item<'a>;

    fn into_par_iter(self) -> Self::Iter {
        unsafe { self.state.par_iter_mut(self.world.archetypes()) }
    }
}
