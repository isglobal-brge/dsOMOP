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
First use also calls this initialization path automatically when the store,
configured UUID and local pin are all absent, including over pre-store upgrade
state. Initialization provisions a noise root on a fresh install and syncs an
owner-only \code{release-store-id} UUID pin in \code{DSOMOP_STATE_DIR}, outside
\code{release-bindings/}. Repeating initialization authenticates existing state
and returns its ID; it never replaces a missing or corrupt established store or
noise root. A configured UUID must match the local pin and authenticated header.
}
\details{
We recommend retaining the returned UUID in deployment configuration outside
\code{DSOMOP_STATE_DIR}, using \code{DSOMOP_DP_RELEASE_STORE_ID} or
\code{dsomop.dp.release_store_id}. Without external pinning, deleting the whole
state root permits a fresh installation with a new file-backed noise root and
release domain; injected roots remain externally controlled. Deleting both
store and local pin while retaining the noise root is a residual only external
UUID pinning prevents. External pinning also detects whole-root loss.

Preserve the complete store, local pin, permanent lock files, noise root, and
any external UUID. Stop all workers before restoration. MACs do not detect
restoring an older complete valid snapshot; avoiding whole-state rollback is a
custodial and storage assumption. Interrupted incomplete initialization requires
custodian recovery. There is no automatic repair or eviction.
}
