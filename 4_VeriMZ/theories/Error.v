Set Implicit Arguments.

Definition bind (A B: Type) (f: option A) (g: A -> option B) : option B :=
  match f with
  | Some x => g x
  | None => None
  end.

Definition bind2 (A B C: Type) (f: option (A * B)) (g: A -> B -> option C) : option C :=
  match f with
  | Some (x, y) => g x y
  | None => None
  end.

Declare Scope error_monad_scope.

Notation "'do' X <- A ; B" := (bind A (fun X => B))
  (at level 200, X ident, A at level 100, B at level 200)
  : error_monad_scope.

Notation "'do' X , Y <- A ; B" := (bind2 A (fun X Y => B))
  (at level 200, X ident, Y ident, A at level 100, B at level 200)
  : error_monad_scope.

Remark bind_inversion:
  forall (A B: Type) (f: option A) (g: A -> option B) (y: B),
  bind f g = Some y ->
  exists x, f = Some x /\ g x = Some y.
Proof.
  intros until y. destruct f; simpl; intros.
  - exists a; tauto.
  - discriminate.
Qed.

Remark bind2_inversion:
  forall (A B C: Type) (f: option (A * B)) (g: A -> B -> option C) (z: C),
  bind2 f g = Some z ->
  exists x y, f = Some (x, y) /\ g x y = Some z.
Proof.
  intros until z. destruct f as [(a, b) |]; simpl; intros.
  - exists a. exists b. tauto.
  - discriminate.
Qed.

Ltac inv_do H :=
  match type of H with
  | (Some _ = Some _) =>
      inversion H; clear H; try subst
  | (None = Some _) =>
      discriminate
  | (bind ?F ?G = Some ?X) =>
      let x := fresh "x" in (
      let EQ1 := fresh "EQ" in (
      let EQ2 := fresh "EQ" in (
      destruct (bind_inversion F G H) as [x [EQ1 EQ2]];
      clear H;
      try (inv_do EQ1);
      try (inv_do EQ2))))
  | (bind2 ?F ?G = Some ?X) =>
      let x1 := fresh "x" in (
      let x2 := fresh "x" in (
      let EQ1 := fresh "EQ" in (
      let EQ2 := fresh "EQ" in (
      destruct (bind2_inversion F G H) as [x1 [x2 [EQ1 EQ2]]];
      clear H;
      try (inv_do EQ1);
      try (inv_do EQ2)))))
  end.