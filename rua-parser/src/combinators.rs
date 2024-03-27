use crate::{Either, Tokens};

use super::{RuaResult, Rule};

#[derive(Clone, Copy)]
pub struct And<L, R> {
    pub left: L,
    pub right: R,
}

#[derive(Clone, Copy)]
pub struct Or<L, R> {
    pub left: L,
    pub right: R,
}

#[derive(Clone, Copy)]
pub struct Map<R, F> {
    pub rule: R,
    pub fun: F,
}

#[derive(Clone, Copy)]
pub struct TryMap<R, F> {
    pub rule: R,
    pub fun: F,
}

#[derive(Clone, Copy)]
pub struct Fold<R, L, F> {
    pub first: R,
    pub rest: L,
    pub fold: F,
}

#[derive(Clone, Copy)]
pub struct Optional<R> {
    pub rule: R,
}

#[derive(Clone, Copy)]
pub struct Switch<F, L, R> {
    pub first: F,
    pub left: L,
    pub right: R,
}

impl<L: Rule, R: Rule> Rule for And<L, R> {
    type Output = (L::Output, R::Output);

    fn parse(self, tokens: &mut impl Tokens) -> RuaResult<Option<Self::Output>> {
        let Some(left) = self.left.parse(tokens)? else {
            return Ok(None);
        };

        let right = self.right.must_parse(tokens)?;

        Ok(Some((left, right)))
    }
}

impl<L, R, Out> Rule for Or<L, R>
where
    L: Rule<Output = Out>,
    R: Rule<Output = Out>,
{
    type Output = Out;

    fn parse(self, tokens: &mut impl Tokens) -> RuaResult<Option<Self::Output>> {
        if let Some(left) = self.left.parse(tokens)? {
            return Ok(Some(left));
        }

        if let Some(right) = self.right.parse(tokens)? {
            return Ok(Some(right));
        }

        Ok(None)
    }
}

impl<L, R> Or<L, R>
where
    L: Rule,
    R: Rule,
{
    pub fn either(self) -> impl Rule<Output = Either<L::Output, R::Output>> {
        Or {
            left: self.left.map(Either::Left),
            right: self.right.map(Either::Right),
        }
    }
}

impl<R, F, Out> Rule for Map<R, F>
where
    R: Rule,
    F: FnOnce(R::Output) -> Out + Copy,
{
    type Output = Out;

    fn parse(self, tokens: &mut impl Tokens) -> RuaResult<Option<Self::Output>> {
        Ok(self.rule.parse(tokens)?.map(self.fun))
    }
}

impl<R, F, Out> Rule for TryMap<R, F>
where
    R: Rule,
    F: FnOnce(R::Output) -> RuaResult<Out> + Copy,
{
    type Output = Out;

    fn parse(self, tokens: &mut impl Tokens) -> RuaResult<Option<Self::Output>> {
        self.rule.parse(tokens)?.map(self.fun).transpose()
    }
}

impl<L, R, F> Rule for Fold<L, R, F>
where
    L: Rule,
    R: Rule,
    F: Fn(L::Output, R::Output) -> L::Output + Copy,
{
    type Output = L::Output;

    fn parse(self, tokens: &mut impl Tokens) -> RuaResult<Option<Self::Output>> {
        let Some(mut first) = self.first.parse(tokens)? else {
            return Ok(None);
        };

        eprintln!("fold parsed one {}", std::any::type_name::<L>());

        while let Some(right) = self.rest.parse(tokens)? {
            eprintln!("fold parsed another");
            first = (self.fold)(first, right);
        }

        Ok(Some(first))
    }
}

impl<R: Rule> Rule for Optional<R> {
    type Output = Option<R::Output>;

    fn parse(self, tokens: &mut impl Tokens) -> RuaResult<Option<Self::Output>> {
        Ok(Some(self.rule.parse(tokens)?))
    }
}

impl<F, L, R, OL, OR> Rule for Switch<F, L, R>
where
    F: Rule<Output = Either<OL, OR>>,
    L: Rule,
    R: Rule,
{
    type Output = Either<(OL, L::Output), (OR, R::Output)>;

    fn parse(self, tokens: &mut impl Tokens) -> RuaResult<Option<Self::Output>> {
        let Some(mut first) = self.first.parse(tokens)? else {
            return Ok(None);
        };

        Ok(Some(match first {
            Either::Left(left) => Either::Left((left, self.left.must_parse(tokens)?)),
            Either::Right(right) => Either::Right((right, self.right.must_parse(tokens)?)),
        }))
    }
}
