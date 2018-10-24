(*
 * Copyright (C) 2015-present David Scott <dave.scott@unikernel.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

(** Block device signatures. *)

type error = Mirage_device.error
(** The type for IO operation errors. *)

val pp_error: error Fmt.t
(** [pp_error] pretty-prints errors. *)

type write_error = [
  | error
  | `Is_read_only      (** attempted to write to a read-only disk *)
]

val pp_write_error: write_error Fmt.t
(** [pp_write_error] pretty-prints errors. *)

type info = {
  read_write: bool;    (** True if we can write, false if read/only *)
  sector_size: int;    (** Octets per sector *)
  size_sectors: int64; (** Total sectors per device *)
}
(** The type for characteristics of the block device. Note some
    devices may be able to make themselves bigger over time. *)

(** Operations on sector-addressible block devices, usually used for
    persistent storage. *)
module type S = sig

  type error = private [> Mirage_device.error]
  (** The type for block errors. *)

  val pp_error: error Fmt.t
  (** [pp_error] is the pretty-printer for errors. *)

  type write_error = private [> Mirage_device.error | `Is_read_only]
  (** The type for write errors. *)

  val pp_write_error: write_error Fmt.t
  (** [pp_write_error] is the pretty-printer for write errors. *)

  include Mirage_device.S

  val get_info: t -> info Lwt.t
  (** Query the characteristics of a specific block device *)

  val read: t -> int64 -> Cstruct.t list -> (unit, error) result Lwt.t
  (** [read device sector_start buffers] reads data starting at
      [sector_start] from the block device into [buffers]. [Ok ()]
      means the buffers have been filled.  [Error _] indicates an I/O
      error has happened and some of the buffers may not be filled.
      Each of elements in the list [buffers] must be a whole number of
      sectors in length.  The list of buffers can be of any length. *)

  val write: t -> int64 -> Cstruct.t list ->
    (unit, write_error) result Lwt.t
  (** [write device sector_start buffers] writes data from [buffers]
      onto the block device starting at [sector_start]. [Ok ()] means
      the contents of the buffers have been written. [Error _]
      indicates a partial failure in which some of the writes may not
      have happened.

      Once submitted, it is not possible to cancel a request and there
      is no timeout.

      The operation may fail with:

      {ul

      {- [`Unimplemented]: the operation has not been implemented, no
      data has been written.}
      {- [`Is_read_only]: the device is read-only, no data has been
      written.}
      {- [`Disconnected]: the device has been disconnected at
        application request, an unknown amount of data has been
        written.}
      }

      Each of [buffers] must be a whole number of sectors in
      length. The list of buffers can be of any length.

      The data will not be copied, so the supplied buffers must not be
      re-used until the IO operation completes. *)

  val discard: t -> int64 -> int64 -> (unit, write_error) result Lwt.t
  (** [discard device sector n] signals that the [n] sectors starting at
      [sector] on [device] are no longer needed and the contents may be
      discarded.
      Reads following the discard will return zeroes.
      Note the contents may not actually be irrecoverable: this is not a
      "secure erase". *)

  val barrier: ?durable:bool -> t -> (unit, write_error) result Lwt.t
  (** [barrier device] will make sure that all writes done prior to the barrier
      call will hit [device] before the writes that are done after the barrier
      io has completed.
      No guarantee is made for writes made during the barrier io.
      If ~durable:true is passed, all writes prior to the barrier
      will be as durable as the device can make them, which may be slower.
      If ~durable is false or omitted, the implementation may still
      rely on slower, durable primitives if these are the only ones
      available. *)
end
