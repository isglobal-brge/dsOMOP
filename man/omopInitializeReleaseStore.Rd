\name{omopInitializeReleaseStore}
\alias{omopInitializeReleaseStore}
\title{Initialize the permanent DP first-answer store}
\usage{omopInitializeReleaseStore(store_id = NULL)}
\arguments{
\item{store_id}{Optional lowercase UUID for a new, unconfigured store. By
default a random UUID is generated. This is not a refresh control.}
}
\value{The store UUID, as a character scalar. It is not a secret.}
\description{
Custodian-only local setup, not a DataSHIELD aggregate or assign method.
Run once with the expected store ID unset, then retain the returned UUID in
deployment configuration outside \code{DSOMOP_STATE_DIR}, using
\code{DSOMOP_DP_RELEASE_STORE_ID} or \code{dsomop.dp.release_store_id}.
Initialization provisions a noise root on a fresh install, as before.
Repeating initialization authenticates existing state and returns its ID;
it never replaces a missing or corrupt established store or noise root.
}
\details{
Preserve the complete store, permanent lock files, noise root, and external
UUID. Stop all workers before restoration. MACs do not detect restoring an
older complete valid snapshot; avoiding whole-state rollback is a custodial
and storage assumption. Interrupted incomplete initialization requires
custodian recovery. There is no automatic repair or eviction.
}
